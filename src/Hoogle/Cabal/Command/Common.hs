{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Distribution.Client.ProjectPlanning.Types (ElaboratedInstallPlan)
import Distribution.Client.ScriptUtils
import Distribution.Client.Setup (GlobalFlags, InstallFlags (..), defaultGlobalFlags)
import Distribution.Client.TargetProblem (TargetProblem)
import Distribution.Client.Types.SourcePackageDb (SourcePackageDb)
import Distribution.Simple (OptimisationLevel (NoOptimisation))
import Distribution.Simple.Setup (ConfigFlags (..), HaddockFlags (..), toFlag)
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
  withContextAndSelectors' RejectNoTargets Nothing flags targetStrings' globalFlags HaddockCommand $ \targetCtx ctx targetSelectors -> do
    let targetAction = TargetActionBuild

    baseCtx <- case targetCtx of
      ProjectContext -> return ctx
      GlobalContext -> return ctx
      ScriptContext path exemeta -> updateContextAndWriteProjectFile ctx path exemeta

    buildCtx <- runProjectPreBuildPhase defaultVerbosity baseCtx $ \elaboratedPlan -> do
      -- Interpret the targets on the command line as build targets
      -- (as opposed to say repl or haddock targets).
      targets <-
        either (reportTargetProblems defaultVerbosity "build") return $
          resolveTargets'
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
            either (die' defaultVerbosity . renderCannotPruneDependencies) return $
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
disableOptimization flags = flags {configOptimization = toFlag NoOptimisation}

setBuildDir :: FilePath -> ConfigFlags -> ConfigFlags
#if MIN_VERSION_Cabal(3,14,0)
setBuildDir buildDir flags =
  flags
    { configCommonFlags =
        (configCommonFlags flags)
          { setupDistPref = toFlag $ makeSymbolicPath buildDir
          }
    }
#else
setBuildDir buildDir flags =
  flags { configDistPref = toFlag buildDir }
#endif

withContextAndSelectors' ::
  AcceptNoTargets ->
  Maybe ComponentKind ->
  NixStyleFlags a ->
  [String] ->
  GlobalFlags ->
  CurrentCommand ->
  ( TargetContext ->
    ProjectBaseContext ->
    [TargetSelector] ->
    IO b
  ) ->
  IO b
#if MIN_VERSION_Cabal(3,16,0)
withContextAndSelectors' = withContextAndSelectors defaultVerbosity
#else
withContextAndSelectors' = withContextAndSelectors
#endif

resolveTargets' ::
  ( forall k.
    TargetSelector ->
    [AvailableTarget k] ->
    Either
      (TargetProblem err)
      [k]
  ) ->
  ( forall k.
    SubComponentTarget ->
    AvailableTarget k ->
    Either (TargetProblem err) k
  ) ->
  ElaboratedInstallPlan ->
  Maybe SourcePackageDb ->
  [TargetSelector] ->
  Either
    [TargetProblem err]
    TargetsMap
#if MIN_VERSION_Cabal(3,16,0)
-- resolveTargetsFromSolver is a drop-in replacement of resolveTargets. See https://github.com/haskell/cabal/commit/0f1f67cb97dca952123f262e7670a200a783acf4
resolveTargets' = resolveTargetsFromSolver
#else
resolveTargets' = resolveTargets
#endif

defaultVerbosity :: Verbosity.Verbosity
defaultVerbosity = Verbosity.normal
