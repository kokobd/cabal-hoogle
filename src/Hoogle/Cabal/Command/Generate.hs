{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hoogle.Cabal.Command.Generate
  ( Log,
    command,
    Command,
    action,
  )
where

import Control.Exception (catch, throw)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (second))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.Extra as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Data.Traversable (forM)
import Distribution.Client.CmdBuild
  ( buildAction,
  )
import Distribution.Client.DistDirLayout (DistDirLayout (distBuildDirectory))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectOrchestration
  ( ProjectBaseContext (distDirLayout),
    ProjectBuildContext (elaboratedPlanToExecute, elaboratedShared, targetsMap),
  )
import Distribution.Client.ProjectPlanning.Types (ElaboratedConfiguredPackage, elabDistDirParams)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (haddockHTMLs, installedUnitId))
import Distribution.Simple (UnitId, pkgName)
import Distribution.Simple.Configure (ConfigStateFileError, tryGetPersistBuildConfig)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Types.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Types.PackageDescription as PackageDescription
import Distribution.Types.PackageName (PackageName, unPackageName)
import qualified Hoogle
import Hoogle.Cabal.Command.Common (Context (..), GlobalOptions (..), hoogleDatabaseArg, readContext)
import Hoogle.Cabal.Logger
import qualified Options.Applicative as OptParse
import System.Directory
  ( createDirectoryIfMissing,
    createDirectoryLink,
    removeDirectoryLink,
    removeDirectoryRecursive,
    withCurrentDirectory,
  )
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (log)

data Log
  = LogBadInstallPlan UnitId (Maybe (InstallPlan.GenericPlanPackage InstalledPackageInfo ElaboratedConfiguredPackage))
  | LogCanNotReadSetupConfig String ConfigStateFileError
  | LogPkgMoreThan1Version PackageName (NonEmpty UnitId)
  | LogPkgBadHaddockHtml PackageName [FilePath]

instance Show Log where
  show (LogBadInstallPlan unitId Nothing) = "can not find " <> show unitId <> " from install plan"
  show (LogBadInstallPlan unitId (Just (InstallPlan.PreExisting _))) = show unitId <> " is PreExisting"
  show (LogBadInstallPlan unitId (Just (InstallPlan.Configured _))) = show unitId <> " is Configured"
  show (LogBadInstallPlan unitId (Just (InstallPlan.Installed _))) = show unitId <> " is Installed"
  show (LogCanNotReadSetupConfig pkg configStateFileErr) =
    [i|Can not read setup-config file from #{pkg}, error: #{configStateFileErr}|]
  show (LogPkgMoreThan1Version pkgName unitIds) =
    [i|Warning: package #{pkgName} has more than 1 version installed, this should not happen. all pkgs: #{unitIds}|]
  show (LogPkgBadHaddockHtml name htmlDirs) =
    [i|Warning: package #{name} doesn't have exactly one haddock html directory, actual: #{htmlDirs}|]

newtype Command = Command
  { _targets :: [String]
  }
  deriving (Show, Eq)

command :: (Command -> a) -> OptParse.Mod OptParse.CommandFields a
command f =
  OptParse.command "generate" $
    OptParse.info
      (fmap f commandParser)
      (OptParse.progDesc "generate hoogle database")

commandParser :: OptParse.Parser Command
commandParser =
  Command
    <$> (OptParse.many . OptParse.strArgument) (OptParse.metavar "TARGETS")

action :: Logger Log -> GlobalOptions -> Command -> IO ()
action logger globalOptions (Command targets) = do
  (Context baseCtx _ hoogleDir targetStrings flags globalFlags) <- readContext globalOptions targets
  buildAction flags targetStrings globalFlags
  Context{..} <- readContext globalOptions targets
  let buildCtx = _context_buildCtx
      targetIds = Map.keys (targetsMap buildCtx)
      installPlan = elaboratedPlanToExecute buildCtx
      hoogleLocalPackagesDir = hoogleDir </> "local"
      hoogleDependenciesDir = hoogleDir </> "dependencies"
  localPackages <- fmap catMaybes . forM targetIds $ \targetId ->
    case InstallPlan.lookup installPlan targetId of
      Just (InstallPlan.Installed pkg) ->
        pure . Just $ distBuildDirectory (distDirLayout baseCtx) $ elabDistDirParams (elaboratedShared buildCtx) pkg
      pkg -> do
        logWith logger Error (LogBadInstallPlan targetId pkg)
        pure Nothing
  catch (removeDirectoryRecursive hoogleDir) $ \(err :: IOError) ->
    if isDoesNotExistError err then pure () else throw err
  createDirectoryIfMissing True hoogleLocalPackagesDir
  createDirectoryIfMissing True hoogleDependenciesDir
  localPackagesBuildInfo <- symlinkLocalPackages logger localPackages hoogleLocalPackagesDir
  let localPkgsName = fmap (pkgName . PackageDescription.package . LocalBuildInfo.localPkgDescr) localPackagesBuildInfo
  dependenciesName <- symlinkDependencies logger localPackagesBuildInfo hoogleDependenciesDir
  let nameStrs = fmap unPackageName (localPkgsName <> dependenciesName)
  withCurrentDirectory hoogleDir $
    Hoogle.hoogle $
      ["generate", hoogleDatabaseArg, "--local=local", "--local=dependencies"] ++ nameStrs

symlinkLocalPackages :: Logger Log -> [FilePath] -> FilePath -> IO [LocalBuildInfo]
symlinkLocalPackages logger localPackages destDir = do
  fmap catMaybes . forM localPackages $ \pkg -> do
    let pkgName = takeFileName pkg
    catch (removeDirectoryLink (destDir </> pkgName)) $ \(e :: IOError) ->
      if isDoesNotExistError e then pure () else throw e
    createDirectoryLink pkg (destDir </> pkgName)
    lbiEither <- tryGetPersistBuildConfig pkg
    case lbiEither of
      Left configStateFileErr -> do
        logWith logger Error $ LogCanNotReadSetupConfig pkgName configStateFileErr
        pure Nothing
      Right lbi -> pure $ Just lbi

symlinkDependencies :: Logger Log -> [LocalBuildInfo] -> FilePath -> IO [PackageName]
symlinkDependencies logger localPackages hoogleDependenciesDir = do
  let nameToPkgs =
        fmap (NonEmpty.nubOrdOn installedUnitId) . Map.fromListWith (<>) $
          concatMap collectDependenciesForPkg localPackages
  pkgs <- fmap catMaybes . forM (Map.toList nameToPkgs) $ \(name, allPkgs@(pkg NonEmpty.:| pkgs)) -> do
    unless (null pkgs) $
      logWith logger Warning $
        LogPkgMoreThan1Version name (fmap installedUnitId allPkgs)
    case haddockHTMLs pkg of
      [htmlDir] -> pure $ Just (name, htmlDir)
      htmlDirs -> do
        logWith logger Warning $ LogPkgBadHaddockHtml name htmlDirs
        pure Nothing
  forM pkgs $ \(name, dir) -> do
    createDirectoryLink dir (hoogleDependenciesDir </> unPackageName name)
    pure name
  where
    collectDependenciesForPkg pkg =
      let depsWithName = allPackagesByName (LocalBuildInfo.installedPkgs pkg)
       in fmap (second (NonEmpty.:| []))
            . concatMap (\(name, pkgs) -> fmap (name,) pkgs)
            $ depsWithName
