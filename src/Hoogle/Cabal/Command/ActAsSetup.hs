{-# LANGUAGE CPP #-}

module Hoogle.Cabal.Command.ActAsSetup
  ( command,
    Command,
    action,
  )
where

import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Make as Make
import Distribution.Parsec (eitherParsec)
import qualified Distribution.Simple as Simple
import Distribution.Types.BuildType
import qualified Options.Applicative as OptParse

data Command = Command
  { _buildType :: String,
    _args :: [String]
  }
  deriving (Show, Eq)

command :: (Command -> a) -> OptParse.Mod OptParse.CommandFields a
command f =
  OptParse.command
    "act-as-setup"
    (OptParse.info (fmap f commandParser) (OptParse.progDesc "(Internal) Run as-if this was a Setup.hs"))

commandParser :: OptParse.Parser Command
commandParser =
  Command
    <$> OptParse.strOption (OptParse.long "build-type")
    <*> (OptParse.many . OptParse.strArgument) (OptParse.metavar "ARGS")

action :: Command -> ExceptT Text IO ()
action (Command buildTypeStr args) = do
  buildType <- liftEither . first T.pack $ eitherParsec buildTypeStr
  case buildType of
    Simple -> liftIO $ Simple.defaultMainArgs args
    Configure ->
      liftIO $
        Simple.defaultMainWithHooksArgs
          Simple.autoconfUserHooks
          args
    Make -> liftIO $ Make.defaultMainArgs args
    Custom -> throwError "Build type 'Custom' not supported"
#if MIN_VERSION_Cabal(3,14,0)
    Hooks -> throwError "Build type 'Hooks' not supported"
#endif
