module Main where

import Lib
import System.Environment
import Data.List
import Diagnostic 


main :: IO ()
main = do putStrLn $ show $ rowOf $ positionOf Failure { text = "Hello", position = Position { col = 5, row = 2 } }
