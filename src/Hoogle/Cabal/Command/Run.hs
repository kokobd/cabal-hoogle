{-# LANGUAGE RecordWildCards #-}

module Hoogle.Cabal.Command.Run
  ( Command,
    command,
    action,
    Log
  )
where

import qualified Hoogle
import Hoogle.Cabal.Command.Common (Context (..), GlobalOptions, hoogleDatabaseArg, readContext)
import Hoogle.Cabal.Logger
import qualified Options.Applicative as OptParse
import System.Directory (doesDirectoryExist, withCurrentDirectory)

newtype Command = Command
  { _hoogleArgs :: [String]
  }
  deriving (Show, Eq)

command :: (Command -> a) -> OptParse.Mod OptParse.CommandFields a
command f = OptParse.command "run" (OptParse.info (fmap f commandParser) (OptParse.progDesc "Run hoogle, with arbitrary arguments"))

commandParser :: OptParse.Parser Command
commandParser = Command <$> (OptParse.many . OptParse.strArgument) (OptParse.metavar "ARGS")

data Log = LogHoogleDirDoesNotExist

instance Show Log where
  show LogHoogleDirDoesNotExist = "please run 'cabal-hoogle generate' first"

action :: Logger Log -> GlobalOptions -> Command -> IO ()
action logger globalOptions Command {..} = do
  Context {..} <- readContext globalOptions []
  hoogleDirExists <- doesDirectoryExist _context_hoogleDir
  if not hoogleDirExists
    then logWith logger Error LogHoogleDirDoesNotExist
    else
      withCurrentDirectory _context_hoogleDir . Hoogle.hoogle $
        hoogleArgs'
  where
    hoogleArgs' = case _hoogleArgs of
      (x : xs) -> x : hoogleDatabaseArg : xs
      [] -> [hoogleDatabaseArg]
