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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.Extra as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.String.Interpolate (i)
import qualified Data.Text.IO as T
import Data.Traversable (forM)
import Distribution.Client.CmdBuild
  ( BuildFlags (buildOnlyConfigure),
    buildAction,
    defaultBuildFlags,
    selectComponentTarget,
    selectPackageTargets,
  )
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import Distribution.Client.DistDirLayout (DistDirLayout (distBuildDirectory, distDirectory))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags)
import Distribution.Client.ProjectOrchestration
  ( BuildTimeSettings (buildSettingOnlyDeps),
    ProjectBaseContext (buildSettings, distDirLayout),
    ProjectBuildContext (elaboratedPlanToExecute, elaboratedShared, targetsMap),
    TargetAction (..),
    pruneInstallPlanToDependencies,
    pruneInstallPlanToTargets,
    resolveTargets,
    runProjectPreBuildPhase,
  )
import Distribution.Client.ProjectPlanning.Types (elabDistDirParams)
import Distribution.Client.ScriptUtils
  ( AcceptNoTargets (RejectNoTargets),
    TargetContext (..),
    updateContextAndWriteProjectFile,
    withContextAndSelectors,
  )
import Distribution.Client.Setup
  ( ConfigFlags (..),
    GlobalFlags,
    InstallFlags (installDocumentation),
    defaultGlobalFlags,
  )
import Distribution.InstalledPackageInfo (InstalledPackageInfo (haddockHTMLs, installedUnitId))
import Distribution.Simple (OptimisationLevel (NoOptimisation), pkgName)
import Distribution.Simple.Configure (tryGetPersistBuildConfig)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.Simple.Setup (Flag (Flag), HaddockFlags (..), fromFlag)
import Distribution.Simple.Utils (die')
import Distribution.Types.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Types.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Types.PackageDescription as PackageDescription
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Verbosity (normal)
import qualified Hoogle
import Hoogle.Cabal.Command.Common (GlobalOptions (..), hoogleDatabaseArg)
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

data Log = Log
  deriving (Show)

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
action logger GlobalOptions {..} (Command targets) = do
  logger <& Log `WithSeverity` Info
  buildAction flags targetStrings globalFlags
  (baseCtx, buildCtx) <- prepareBuildContext flags targetStrings globalFlags
  let targetIds = Map.keys (targetsMap buildCtx)
      installPlan = elaboratedPlanToExecute buildCtx
      localPackages = flip fmap targetIds $ \targetId ->
        let InstallPlan.Installed pkg = fromJust $ InstallPlan.lookup installPlan targetId
            distDirParams = elabDistDirParams (elaboratedShared buildCtx) pkg
         in distBuildDirectory (distDirLayout baseCtx) distDirParams
      hoogleDir = distDirectory (distDirLayout baseCtx) </> ".hoogle"
      hoogleLocalPackagesDir = hoogleDir </> "local"
      hoogleDependenciesDir = hoogleDir </> "dependencies"
  catch (removeDirectoryRecursive hoogleDir) $ \(err :: IOError) ->
    if isDoesNotExistError err then pure () else throw err
  createDirectoryIfMissing True hoogleLocalPackagesDir
  createDirectoryIfMissing True hoogleDependenciesDir
  localPackagesBuildInfo <- symlinkLocalPackages localPackages hoogleLocalPackagesDir
  let localPkgsName = fmap (pkgName . PackageDescription.package . LocalBuildInfo.localPkgDescr) localPackagesBuildInfo
  dependenciesName <- symlinkDependencies localPackagesBuildInfo hoogleDependenciesDir
  let nameStrs = fmap unPackageName (localPkgsName <> dependenciesName)
  withCurrentDirectory hoogleDir $
    Hoogle.hoogle $
      ["generate", hoogleDatabaseArg, "--local=local", "--local=dependencies"] ++ nameStrs

  pure ()
  where
    defaultFlags = defaultNixStyleFlags defaultBuildFlags
    flags =
      defaultFlags
        { configFlags =
            (configFlags defaultFlags)
              { configOptimization = Flag NoOptimisation,
                configDistPref = Flag _globalOptions_builddir
              },
          haddockFlags =
            (haddockFlags defaultFlags)
              { haddockHoogle = Flag True,
                haddockHtml = Flag True,
                haddockLinkedSource = Flag True,
                haddockQuickJump = Flag True
              },
          installFlags =
            (installFlags defaultFlags)
              { installDocumentation = Flag True
              }
        }
    targetStrings :: [String]
    targetStrings = if null targets then ["all"] else targets
    globalFlags = defaultGlobalFlags

prepareBuildContext ::
  NixStyleFlags BuildFlags ->
  [String] ->
  GlobalFlags ->
  IO (ProjectBaseContext, ProjectBuildContext)
prepareBuildContext flags@NixStyleFlags {extraFlags = buildFlags} targetStrings globalFlags =
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings globalFlags $ \targetCtx ctx targetSelectors -> do
    -- TODO: This flags defaults business is ugly
    let onlyConfigure =
          fromFlag
            ( buildOnlyConfigure defaultBuildFlags
                <> buildOnlyConfigure buildFlags
            )
        targetAction
          | onlyConfigure = TargetActionConfigure
          | otherwise = TargetActionBuild

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    buildCtx <- runProjectPreBuildPhase normal baseCtx $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      targets <-
        either (reportTargetProblems normal "build") return $
          resolveTargets
            selectPackageTargets
            selectComponentTarget
            elaboratedPlan
            Nothing
            targetSelectors

      let elaboratedPlan' =
            pruneInstallPlanToTargets
              targetAction
              targets
              elaboratedPlan
      elaboratedPlan'' <-
        if buildSettingOnlyDeps (buildSettings baseCtx)
          then
            either (die' normal . renderCannotPruneDependencies) return $
              pruneInstallPlanToDependencies
                (Map.keysSet targets)
                elaboratedPlan'
          else return elaboratedPlan'

      return (elaboratedPlan'', targets)
    pure (baseCtx, buildCtx)

symlinkLocalPackages :: [FilePath] -> FilePath -> IO [LocalBuildInfo]
symlinkLocalPackages localPackages destDir = do
  fmap catMaybes . forM localPackages $ \pkg -> do
    catch (removeDirectoryLink (destDir </> takeFileName pkg)) $ \(e :: IOError) ->
      if isDoesNotExistError e then pure () else throw e
    createDirectoryLink pkg (destDir </> takeFileName pkg)
    lbiEither <- tryGetPersistBuildConfig pkg
    case lbiEither of
      Left configStateFileErr -> do
        T.putStrLn [i|Can not read setup-config file from #{pkg}, error: #{configStateFileErr}|]
        pure Nothing
      Right lbi -> pure $ Just lbi

symlinkDependencies :: [LocalBuildInfo] -> FilePath -> IO [PackageName]
symlinkDependencies localPackages hoogleDependenciesDir = do
  let nameToPkgs =
        fmap (NonEmpty.nubOrdOn installedUnitId) . Map.fromListWith (<>) $
          concatMap collectDependenciesForPkg localPackages
  pkgs <- fmap catMaybes . forM (Map.toList nameToPkgs) $ \(name, allPkgs@(pkg NonEmpty.:| pkgs)) -> do
    unless (null pkgs) $
      T.putStrLn [i|Warning: package #{name} has more than 1 version installed, this should not happen. all pkgs: #{fmap installedUnitId allPkgs}|]
    case haddockHTMLs pkg of
      [htmlDir] -> pure $ Just (name, htmlDir)
      htmlDirs -> do
        T.putStrLn [i|Warning: package #{name} doesn't have exactly one haddock html directory, actual: #{htmlDirs}|]
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
