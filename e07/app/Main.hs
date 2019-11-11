module Main where

import AST
import Lib
import System.Environment
import Data.List
import Diagnostic 
import Handler
import Token
import Position
import Lexer
import System.Directory (doesFileExist)

main :: IO ()
main = do print $ "hi"