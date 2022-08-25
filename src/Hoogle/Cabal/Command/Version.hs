{-# LANGUAGE CPP #-}

module Hoogle.Cabal.Command.Version
  ( Command (..),
    command,
    action,
  )
where

import qualified Options.Applicative as OptParse

data Command = Command
  deriving (Show, Eq)

command :: (Command -> a) -> OptParse.Mod OptParse.CommandFields a
command f =
  OptParse.command "version" $
    OptParse.info (pure (f Command)) (OptParse.progDesc "Show version")

action :: Command -> IO ()
action _ = do
  putStrLn $ "cabal-hoogle version " ++ VERSION_cabal_hoogle
  putStrLn $ "compiled using version " ++ VERSION_Cabal ++ " of the Cabal library"
