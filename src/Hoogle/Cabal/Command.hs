{-# LANGUAGE RecordWildCards #-}

module Hoogle.Cabal.Command where

import qualified Hoogle.Cabal.Command.ActAsSetup as ActAsSetup
import Hoogle.Cabal.Command.Common
import qualified Hoogle.Cabal.Command.Generate as Generate
import Hoogle.Cabal.Logger
import Options.Applicative

data CmdOptions = CmdOptions
  { _cmdOptions_global :: GlobalOptions,
    _cmdOptions_command :: Command
  }
  deriving (Show, Eq)

data Command
  = CommandGenerate Generate.Command
  | CommandRun [String]
  | CommandActAsSetup ActAsSetup.Command
  deriving (Show, Eq)

data Log
  = LogGenerate Generate.Log
  | LogCommandBegin
  | LogCommandDone

instance Show Log where
  show (LogGenerate l) = show l
  show LogCommandBegin = "command execution begins"
  show LogCommandDone = "command execution done"

executeCommand :: Logger Log -> IO ()
executeCommand logger = do
  logger <& LogCommandBegin `WithSeverity` Info
  CmdOptions {..} <- readCmdOptions
  case _cmdOptions_command of
    CommandGenerate cmd -> Generate.action (cmapLogger LogGenerate logger) _cmdOptions_global cmd
    CommandActAsSetup cmd -> ActAsSetup.action cmd
    CommandRun cmd -> undefined
  logger <& LogCommandDone `WithSeverity` Info

parser :: Parser CmdOptions
parser =
  CmdOptions
    <$> globalOptionsParser
    <*> hsubparser
      ( Generate.command CommandGenerate
          <> command "run" (info commandRunParser (progDesc "run hoogle, with arbitrary arguments"))
          <> ActAsSetup.command CommandActAsSetup
      )

commandRunParser :: Parser Command
commandRunParser = CommandRun <$> (many . strArgument) (metavar "ARGS")

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