module Main where

import AST
import Lib
import System.Environment
import Data.List
import Diagnostic 
import Handler

main :: IO ()
main = do 
  args <- getArgs
  print $ "Hello"
