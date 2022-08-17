module Hoogle.Cabal.Command.Run where

newtype Command = Command
  { _hoogleArgs :: [String]
  }
  deriving (Show, Eq)

