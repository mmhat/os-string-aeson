module Main (main) where

import Prelude

import System.Environment (getArgs, getProgName, withArgs)
import Test.Tasty (
    TestTree,
    defaultIngredients,
    defaultMainWithIngredients,
    testGroup,
 )
import Test.Tasty.Ingredients.Rerun (rerunningTests)

import Properties.Posix qualified
import Properties.Windows qualified
import Units.Posix qualified
import Units.Windows qualified

main :: IO ()
main = do
    name <- getProgName
    args <- getArgs
    let
        defaultArgs = ["--rerun", "--rerun-log-file", ".tasty-rerun-log." <> name]
    withArgs (defaultArgs <> args) $
        defaultMainWithIngredients [rerunningTests defaultIngredients] tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup
            "Posix"
            [ Units.Posix.tests
            , Properties.Posix.tests
            ]
        , testGroup
            "Windows"
            [ Units.Windows.tests
            , Properties.Windows.tests
            ]
        ]
