{-# LANGUAGE RecordWildCards #-}

module Hoogle.Cabal.Command where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Hoogle.Cabal.Command.ActAsSetup as ActAsSetup
import Hoogle.Cabal.Command.Common
import qualified Hoogle.Cabal.Command.Generate as Generate
import qualified Hoogle.Cabal.Command.Run as Run
import qualified Hoogle.Cabal.Command.Version as Version
import Hoogle.Cabal.Logger
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (stderr)

data CmdOptions = CmdOptions
  { _cmdOptions_global :: GlobalOptions,
    _cmdOptions_command :: Maybe Command
  }
  deriving (Show, Eq)

data Command
  = CommandGenerate Generate.Command
  | CommandRun Run.Command
  | CommandActAsSetup ActAsSetup.Command
  | CommandVersion Version.Command
  deriving (Show, Eq)

data Log
  = LogGenerate Generate.Log
  | LogRun Run.Log

instance Show Log where
  show (LogGenerate l) = show l
  show (LogRun l) = show l

executeCommand :: Logger Log -> IO ()
executeCommand logger = do
  CmdOptions {..} <- readCmdOptions
  if _globalOptions_version _cmdOptions_global
    then Version.action Version.Command
    else case _cmdOptions_command of
      Nothing -> pure ()
      Just cmd' -> case cmd' of
        CommandGenerate cmd -> Generate.action (cmapLogger LogGenerate logger) _cmdOptions_global cmd
        CommandActAsSetup cmd -> handleError $ ActAsSetup.action cmd
        CommandRun cmd -> Run.action (cmapLogger LogRun logger) _cmdOptions_global cmd
        CommandVersion cmd -> Version.action cmd

parser :: Parser CmdOptions
parser =
  CmdOptions
    <$> globalOptionsParser
    <*> optional
      ( hsubparser
          ( Generate.command CommandGenerate
              <> Run.command CommandRun
              <> Version.command CommandVersion
              <> ActAsSetup.command CommandActAsSetup
          )
      )

readCmdOptions :: IO CmdOptions
readCmdOptions = execParser parserInfo
  where
    parserInfo =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc
              ( "Run hoogle on your local packages and dependencies. "
                  <> "See https://github.com/kokobd/cabal-hoogle for more information"
              )
        )

handleError :: ExceptT Text IO a -> IO a
handleError m = do
  resultEither <- runExceptT m
  case resultEither of
    Left errMsg -> do
      TIO.hPutStrLn stderr errMsg
      exitFailure
    Right result -> pure result

