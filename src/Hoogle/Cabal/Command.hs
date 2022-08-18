{-# LANGUAGE RecordWildCards #-}

module Hoogle.Cabal.Command where

import qualified Hoogle.Cabal.Command.ActAsSetup as ActAsSetup
import Hoogle.Cabal.Command.Common
import qualified Hoogle.Cabal.Command.Generate as Generate
import qualified Hoogle.Cabal.Command.Run as Run
import Hoogle.Cabal.Logger
import Options.Applicative

data CmdOptions = CmdOptions
  { _cmdOptions_global :: GlobalOptions,
    _cmdOptions_command :: Command
  }
  deriving (Show, Eq)

data Command
  = CommandGenerate Generate.Command
  | CommandRun Run.Command
  | CommandActAsSetup ActAsSetup.Command
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
  case _cmdOptions_command of
    CommandGenerate cmd -> Generate.action (cmapLogger LogGenerate logger) _cmdOptions_global cmd
    CommandActAsSetup cmd -> ActAsSetup.action cmd
    CommandRun cmd -> Run.action (cmapLogger LogRun logger) _cmdOptions_global cmd

parser :: Parser CmdOptions
parser =
  CmdOptions
    <$> globalOptionsParser
    <*> hsubparser
      ( Generate.command CommandGenerate
          <> Run.command CommandRun
          <> ActAsSetup.command CommandActAsSetup
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