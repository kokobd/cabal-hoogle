{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hoogle.Cabal (main) where

import Control.Exception (catch, throw)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.Extra as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (forM)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (haddockHTMLs, installedUnitId))
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, installedPkgs, localPkgDescr)
import Distribution.Types.PackageDescription (PackageDescription (package))
import Distribution.Types.PackageId (pkgName)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Hoogle.Cabal.CmdOptions
import System.Directory (createDirectoryIfMissing, createDirectoryLink, listDirectory, makeAbsolute, removeDirectoryLink, removeDirectoryRecursive, withCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)
import System.Process.Typed
import Text.Regex.TDFA ((=~))
import Prelude hiding (log)

main :: IO ()
main = do
  CmdOptions {..} <- readCmdOptions
  localPackagesDir <- findLocalPackagesBuildDir cmdOptions_compiler cmdOptions_platform cmdOptions_builddir
  let hoogleDir = localPackagesDir </> ".hoogle"
      hoogleLocalPackagesDir = hoogleDir </> "local"
      hoogleDependenciesDir = hoogleDir </> "dependencies"

  -- Generate (TODO: make this step optional)
  catch (removeDirectoryRecursive hoogleDir) $ \(err :: IOError) ->
    if isDoesNotExistError err then pure () else throw err
  createDirectoryIfMissing True hoogleLocalPackagesDir
  createDirectoryIfMissing True hoogleDependenciesDir
  localPackages <- symlinkLocalPackages localPackagesDir hoogleLocalPackagesDir
  let localPkgsName = fmap (pkgName . package . localPkgDescr) localPackages
  dependenciesName <- symlinkDependencies localPackages hoogleDependenciesDir
  let nameStrs = fmap unPackageName (localPkgsName <> dependenciesName)
  withCurrentDirectory hoogleDir $ do
    runProcess_ . proc "hoogle" $
      ["generate", "--database=all.hoo", "--local=local", "--local=dependencies"] ++ nameStrs
    -- Serve
    runProcess_ . proc "hoogle" $
      ["server", "--database=all.hoo", "--local", "--port=8080"]

  pure ()

findLocalPackagesBuildDir ::
  -- | compiler
  Maybe Text ->
  -- | platform
  Maybe Text ->
  -- | cabal project build dir (often dist-newstyle)
  FilePath ->
  IO FilePath
findLocalPackagesBuildDir compiler platform projectBuildDir = do
  -- directory layout: dist-newstyle/build/x86_64-linux/ghc-9.2.3/$PACKAGE_NAME
  projectBuildDir' <- makeAbsolute projectBuildDir
  platformDir <- enterSubDir (projectBuildDir' </> "build") "platform" (fmap T.unpack platform)
  enterSubDir platformDir "compiler" (fmap T.unpack compiler)

exitOnError :: Text -> IO a
exitOnError log = do
  T.hPutStrLn stderr log
  exitFailure

enterSubDir :: FilePath -> Text -> Maybe FilePath -> IO FilePath
enterSubDir baseDir realm givenSubDir = do
  subDirs <- listDirectory baseDir
  case givenSubDir >>= (\d -> if d `notElem` subDirs then Just d else Nothing) of
    Just wrongSubDir -> exitOnError [i|specified #{realm} #{wrongSubDir} doesn't not exist, make sure to build first|]
    Nothing -> case subDirs of
      [exactlyOne] -> pure $ baseDir </> exactlyOne
      _ -> exitOnError [i|fail to guess #{realm}, please specify one|]

symlinkLocalPackages :: FilePath -> FilePath -> IO [LocalBuildInfo]
symlinkLocalPackages localPackagesDir destDir = do
  localPackages <- filter (=~ packageNameRegex) <$> listDirectory localPackagesDir
  forM localPackages $ \pkg -> do
    catch (removeDirectoryLink (destDir </> pkg)) $ \(e :: IOError) ->
      if isDoesNotExistError e then pure () else throw e
    createDirectoryLink (localPackagesDir </> pkg) (destDir </> pkg)
    getPersistBuildConfig (localPackagesDir </> pkg)
  where
    packageNameRegex :: String =
      "[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*-[0-9]+([.][0-9]+)*"

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
      let depsWithName = allPackagesByName (installedPkgs pkg)
       in fmap (second (NonEmpty.:| []))
            . concatMap (\(name, pkgs) -> fmap (name,) pkgs)
            $ depsWithName
