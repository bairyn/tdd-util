module Main where

import Test.Util.Framework

import Tests

main :: IO ()
main = do
    defaultMain tests
