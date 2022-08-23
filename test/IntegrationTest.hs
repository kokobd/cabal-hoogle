module IntegrationTest where

import Data.List (isInfixOf)
import Hoogle.Cabal
import System.Environment (withArgs)
import System.IO.Silently (capture_)
import Test.Tasty
import Test.Tasty.HUnit

test_searchInCommandLine :: IO [TestTree]
test_searchInCommandLine = do
  withArgs ["generate"] main

  let testCase' :: String -> [String] -> String -> TestTree
      testCase' name args wantResult =
        testCase name $ do
          output <- withArgs (["run", "--"] ++ args) (capture_ main)
          wantResult `isInfixOf` output @? "actual output: " <> output

  pure
    [ testCase' "test dependency" ["testCase"] "Test.QuickCheck failingTestCase :: Result -> [String]",
      testCase' "core library" ["lift"] "Control.Monad.Trans.Class lift",
      testCase' "non-core library" ["progDesc"] "Options.Applicative progDesc :: String -> InfoMod a"
    ]
