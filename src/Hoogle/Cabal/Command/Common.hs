{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Hoogle.Cabal.Command.Common
  ( GlobalOptions (..),
    globalOptionsParser,
    hoogleDatabaseArg,
    Context (..),
    readContext,
  )
where

import Data.Map.Strict qualified as Map
import Distribution.Client.CmdBuild (BuildFlags, defaultBuildFlags, selectComponentTarget, selectPackageTargets)
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import Distribution.Client.DistDirLayout (distDirectory)
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ScriptUtils
import Distribution.Client.Setup (GlobalFlags, InstallFlags (..), defaultGlobalFlags)
import Distribution.Simple (OptimisationLevel (NoOptimisation))
import Distribution.Simple.Setup (ConfigFlags (..), HaddockFlags (..), toFlag)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.Setup (setupDistPref)
import Distribution.Utils.Path (makeSymbolicPath)
#endif
import Distribution.Simple.Utils (die')
import Distribution.Verbosity qualified as Verbosity
import GHC.Generics (Generic)
import Options.Applicative
import System.FilePath ((</>))

data GlobalOptions = GlobalOptions
  { _globalOptions_builddir :: FilePath,
    _globalOptions_version :: Bool
  }
  deriving stock (Show, Eq, Generic)

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
  _globalOptions_builddir <-
    strOption
      ( long "builddir"
          <> value "dist-newstyle/hoogle"
          <> help "Build dir for cabal-hoogle (default: dist-newstyle/hoogle)"
      )
  _globalOptions_version <-
    switch
      ( long "version"
          <> help "Print version"
      )
  pure GlobalOptions {..}

hoogleDatabaseArg :: String
hoogleDatabaseArg = "--database=all.hoo"

data Context = Context
  { _context_baseCtx :: ProjectBaseContext,
    _context_buildCtx :: ProjectBuildContext,
    _context_hoogleDir :: FilePath,
    _context_targetStrings :: [String],
    _context_flags :: NixStyleFlags BuildFlags,
    _context_globalFlags :: GlobalFlags
  }
  deriving stock (Generic)

readContext ::
  GlobalOptions ->
  [String] ->
  IO Context
readContext GlobalOptions {_globalOptions_builddir} targetStrings =
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings' globalFlags HaddockCommand $ \targetCtx ctx targetSelectors -> do
    let targetAction = TargetActionBuild

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    let verbosity = Verbosity.normal
    buildCtx <- runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
      targets <-
        either (reportTargetProblems verbosity "build") return $
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
            either (die' verbosity . renderCannotPruneDependencies) return $
              pruneInstallPlanToDependencies
                (Map.keysSet targets)
                elaboratedPlan'
          else return elaboratedPlan'

      return (elaboratedPlan'', targets)
    pure $
      Context
        baseCtx
        buildCtx
        (distDirectory (distDirLayout baseCtx) </> ".hoogle")
        targetStrings'
        flags
        globalFlags
  where
    defaultFlags = defaultNixStyleFlags defaultBuildFlags
    flags =
      defaultFlags
        { configFlags = setBuildDir _globalOptions_builddir . disableOptimization $ configFlags defaultFlags,
          haddockFlags =
            (haddockFlags defaultFlags)
              { haddockHoogle = toFlag True,
                haddockHtml = toFlag True,
                haddockLinkedSource = toFlag True,
                haddockQuickJump = toFlag True
              },
          installFlags =
            (installFlags defaultFlags)
              { installDocumentation = toFlag True
              }
        }
    targetStrings' :: [String]
    targetStrings' = if null targetStrings then ["all"] else targetStrings
    globalFlags = defaultGlobalFlags

disableOptimization :: ConfigFlags -> ConfigFlags
disableOptimization configFlags = configFlags {configOptimization = toFlag NoOptimisation}

setBuildDir :: FilePath -> ConfigFlags -> ConfigFlags
#if MIN_VERSION_Cabal(3,14,0)
setBuildDir buildDir configFlags =
  configFlags
    { configCommonFlags =
        (configCommonFlags configFlags)
          { setupDistPref = toFlag (makeSymbolicPath buildDir)
          }
    }
#else
setBuildDir buildDir configFlags =
  configFlags
    { configDistPref = toFlag buildDir
    }
#endif
