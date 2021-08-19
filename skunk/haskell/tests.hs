module Main where

import qualified TestParser as TP
import Test.Tasty

import System.Exit

main :: IO ()
main = do 
  good <- and <$> sequence [TP.runTests]

  if good
    then defaultMain TP.parserTests
    else exitFailure