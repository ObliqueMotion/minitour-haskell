module Main where

import AST
import Lib
import System.Environment
import Data.List
import Diagnostic 
import Handler
import Lexer
import System.Directory (doesFileExist)

main :: IO ()
main = do let path = "test.mini"
          fileExists <- doesFileExist path
          if fileExists then do 
              input <- readFile path
              print input
              print $ Lexer.lex input
          else
              print "No file"