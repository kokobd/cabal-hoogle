{-# LANGUAGE ScopedTypeVariables #-}

module Hoogle.Cabal (main) where

import Hoogle.Cabal.Command (executeCommand)
import Hoogle.Cabal.Logger (stdoutLogger)

main :: IO ()
main = executeCommand stdoutLogger
