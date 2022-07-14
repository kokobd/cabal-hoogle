module Hoogle.Cabal (main) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.Pretty (pretty)
import Distribution.Simple.Configure (getInstalledPackages, getPersistBuildConfig)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.System (buildPlatform)
import Distribution.Types.InstalledPackageInfo (haddockHTMLs)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (compiler, withPackageDB, withPrograms))
import Distribution.Verbosity (verbose)
import Hoogle.Cabal.CmdOptions
import System.Directory (listDirectory, makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath
import System.IO (stderr)
import Prelude hiding (log)
import Data.String.Interpolate (i)

main :: IO ()
main = do
  cmdOptions <- readCmdOptions
  localPackagesDir <- findLocalPackagesBuildDir cmdOptions.compiler cmdOptions.platform cmdOptions.builddir
  print localPackagesDir

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
