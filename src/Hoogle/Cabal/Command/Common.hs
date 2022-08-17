module Hoogle.Cabal.Command.Common
  ( GlobalOptions (..),
    globalOptionsParser
  )
where

import Options.Applicative
import System.FilePath ((</>))

newtype GlobalOptions = GlobalOptions
  { _globalOptions_builddir :: FilePath
  }
  deriving (Show, Eq)

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> strOption
      ( long "builddir"
          <> value "dist-newstyle/hoogle"
          <> help "cabal project build dir"
      )