module Main where

import APITest (apiTests)
import ReportTest (reportTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Jizhang" [reportTests, apiTests]
