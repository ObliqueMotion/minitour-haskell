module Main where

import System.Environment
import Data.List
import Lib

main :: IO ()
main = do 
  args <- getArgs
  compiler args
