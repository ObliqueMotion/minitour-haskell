module Main where

import System.Environment
import Data.List
import Lib
import Position


main :: IO ()
main = do putStrLn $ show $ coordString Position { col = 5, row = 2 }
