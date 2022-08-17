{-# LANGUAGE ScopedTypeVariables #-}

module Hoogle.Cabal (main) where

import Hoogle.Cabal.Command (executeCommand)
import Hoogle.Cabal.Logger (stdoutLogger)

main :: IO ()
main = executeCommand stdoutLogger

-- args <- getArgs
-- putStrLn $ "args: " <> show args
-- options@CmdOptions {..} <- readCmdOptions
-- localPackagesDir <- findLocalPackagesBuildDir cmdOptions_compiler cmdOptions_platform cmdOptions_builddir
-- let hoogleDir = localPackagesDir </> ".hoogle"
--     hoogleLocalPackagesDir = hoogleDir </> "local"
--     hoogleDependenciesDir = hoogleDir </> "dependencies"

-- case cmdOptions_command of
--   CommandActAsSetup _ args -> actAsSetupAction (ActAsSetupFlags (Flag Simple)) args
--   CommandGenerate -> generateCommand
--   -- catch (removeDirectoryRecursive hoogleDir) $ \(err :: IOError) ->
--   --   if isDoesNotExistError err then pure () else throw err
--   -- createDirectoryIfMissing True hoogleLocalPackagesDir
--   -- createDirectoryIfMissing True hoogleDependenciesDir
--   -- localPackages <- symlinkLocalPackages localPackagesDir hoogleLocalPackagesDir
--   -- let localPkgsName = fmap (pkgName . package . localPkgDescr) localPackages
--   -- dependenciesName <- symlinkDependencies localPackages hoogleDependenciesDir
--   -- let nameStrs = fmap unPackageName (localPkgsName <> dependenciesName)
--   -- withCurrentDirectory hoogleDir $ do
--   --   runProcess_ . proc "hoogle" $
--   --     ["generate", databaseArg, "--local=local", "--local=dependencies"] ++ nameStrs
--   CommandRun hoogleArgs -> do
--     let hoogleArgs' = case hoogleArgs of
--           (x : xs) -> x : databaseArg : xs
--           [] -> [databaseArg]
--     withCurrentDirectory hoogleDir . runProcess_ . proc "hoogle" $
--       hoogleArgs'
-- where
--   databaseArg = "--database=all.hoo"
