module Main where

import Lib
import System.Environment
import Data.List
import Diagnostic 
import Handler


main :: IO ()
main = do putStrLn $ show $ Failure { text = "Hello", position = Position { row = 5, col = 2 } }
