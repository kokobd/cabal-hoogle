module Hoogle.Cabal.CmdOptions
  ( CmdOptions (..),
    readCmdOptions,
    Command (..),
  )
where

import Data.Text (Text)
import Options.Applicative

data CmdOptions = CmdOptions
  { cmdOptions_compiler :: Maybe Text,
    cmdOptions_platform :: Maybe Text,
    cmdOptions_builddir :: FilePath,
    cmdOptions_command :: Command
  }
  deriving (Show, Eq)

data Command
  = CommandGenerate
  | CommandRun [String]
  deriving (Show, Eq)

parser :: Parser CmdOptions
parser =
  CmdOptions
    <$> (optional . strOption)
      ( long "compiler"
          <> help "compiler name and version, for example 'ghc-9.2.3'"
      )
    <*> (optional . strOption)
      ( long "platform"
          <> help "platform, for example x86_64-linux"
      )
    <*> strOption
      ( long "builddir"
          <> value "dist-newstyle"
          <> help "cabal project build dir"
      )
    <*> hsubparser
      ( command "generate" (info (pure CommandGenerate) (progDesc "generate hoogle database"))
          <> command "run" (info commandRunParser (progDesc "run hoogle, with arbitrary arguments"))
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