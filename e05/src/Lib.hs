module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position
import Data.Char (ord)
import Data.List (intercalate)

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

displayDiagnostics :: Handler -> IO ()
displayDiagnostics = putStrLn . show

lineNumber :: Int -> String
lineNumber x = (show x) ++ ":    "

numberedLines :: String -> String
numberedLines s = concat $ zipWith (++) lineNumbers lineStrings
    where lineNumbers = map lineNumber [1..]
          lineStrings = map (++ "\n") $ lines s


compiler :: [String] -> IO ()
compiler [ ]         = displayDiagnostics $ report handler (failure "Too few arguments." (position 0 0))
compiler (x1:x2:xs)  = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    input <- readFile $ fileName ++ ".mini"
    putStrLn $ numberedLines input