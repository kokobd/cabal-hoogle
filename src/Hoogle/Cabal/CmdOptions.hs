module Hoogle.Cabal.CmdOptions
  ( CmdOptions(..),
    readCmdOptions,
  )
where

import Data.Text (Text)
import Options.Applicative

data CmdOptions = CmdOptions
  { compiler :: Maybe Text,
    platform :: Maybe Text,
    builddir :: FilePath,
    hoogleArgs :: [Text]
  } deriving (Show, Eq)

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
          <> help "cabal builddir"
      )
    <*> (many . strArgument)
      ( metavar "HOOGLE-ARGS"
      )

readCmdOptions :: IO CmdOptions
readCmdOptions = execParser parserInfo
  where
    parserInfo =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Run hoogle on your local packages and dependencies"
        )