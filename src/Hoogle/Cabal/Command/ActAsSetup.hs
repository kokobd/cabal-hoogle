module Hoogle.Cabal.Command.ActAsSetup
  ( command,
    Command,
    action,
  )
where

import Data.Maybe (fromJust)
import qualified Distribution.Make as Make
import Distribution.Parsec (simpleParsec)
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
  (OptParse.internal <>) $
    OptParse.command "act-as-setup" $
      OptParse.info (fmap f commandParser) (OptParse.progDesc "Run as-if this was a Setup.hs")

commandParser :: OptParse.Parser Command
commandParser =
  Command
    <$> OptParse.strOption (OptParse.long "build-type")
    <*> (OptParse.many . OptParse.strArgument) (OptParse.metavar "ARGS")

action :: Command -> IO ()
action (Command buildTypeStr args) =
  let bt = fromJust $ simpleParsec buildTypeStr -- TODO: report error properly
   in case bt of
        Simple -> Simple.defaultMainArgs args
        Configure ->
          Simple.defaultMainWithHooksArgs
            Simple.autoconfUserHooks
            args
        Make -> Make.defaultMainArgs args
        Custom -> error "actAsSetupAction Custom"