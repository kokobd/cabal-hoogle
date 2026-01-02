{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hoogle.Cabal.Command.Common
  ( GlobalOptions (..),
    globalOptionsParser,
    hoogleDatabaseArg,
    Context (..),
    readContext,
  )
where

import qualified Data.Map.Strict as Map
import Distribution.Client.CmdBuild (BuildFlags, defaultBuildFlags, selectComponentTarget, selectPackageTargets)
import Distribution.Client.CmdErrorMessages (renderCannotPruneDependencies, reportTargetProblems)
import Distribution.Client.DistDirLayout (distDirectory)
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ScriptUtils
import Distribution.Client.Setup (GlobalFlags, InstallFlags (..), defaultGlobalFlags)
import Distribution.Simple (OptimisationLevel (NoOptimisation))
import Distribution.Simple.Setup (ConfigFlags (..), Flag (..), HaddockFlags (..))
import Distribution.Simple.Utils (die')
import qualified Distribution.Verbosity as Verbosity
import Options.Applicative
import System.FilePath ((</>))
# if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple.Setup (CommonSetupFlags (..))
import Distribution.Utils.Path (makeSymbolicPath)
#endif

data GlobalOptions = GlobalOptions
  { _globalOptions_builddir :: FilePath,
    _globalOptions_version :: Bool
  }
  deriving (Show, Eq)

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> strOption
      ( long "builddir"
          <> value "dist-newstyle/hoogle"
          <> help "Cabal project build dir"
      )
    <*> switch
      ( long "version"
          <> help "Print version"
      )

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

-- | This is copied from the implementation of 'buildAction'
readContext ::
  GlobalOptions ->
  [String] ->
  IO Context
readContext GlobalOptions {..} targetStrings =
  withContextAndSelectors RejectNoTargets Nothing flags targetStrings' globalFlags HaddockCommand $ \targetCtx ctx targetSelectors -> do
    let targetAction = TargetActionBuild

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    let verbosity = Verbosity.normal
    buildCtx <- runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
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
#if MIN_VERSION_Cabal(3,14,0)
        { configFlags =
            (configFlags defaultFlags)
              { configOptimization = Flag NoOptimisation
              , configCommonFlags =
                (configCommonFlags (configFlags defaultFlags))
                { setupDistPref = Flag (makeSymbolicPath _globalOptions_builddir)
                }
              },
#else
        { configFlags =
            (configFlags defaultFlags)
              { configOptimization = Flag NoOptimisation,
                configDistPref = Flag _globalOptions_builddir
              },
#endif
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
    targetStrings' :: [String]
    targetStrings' = if null targetStrings then ["all"] else targetStrings
    globalFlags = defaultGlobalFlags
