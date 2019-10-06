module Main where

import Lib
import System.Environment
import Data.List
import Diagnostic 
import Handler

main :: IO ()
main = do 
  args <- getArgs
  compiler args
