-- | Main module to run all tests.
--
module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Text.Blaze.Tests

main :: IO ()
main = defaultMain
    [ testGroup "Text.Blaze.Tests" Text.Blaze.Tests.tests
    ]
