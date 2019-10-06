module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position
import Data.Char (ord)

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

displayDiagnostics :: Handler -> IO ()
displayDiagnostics = putStrLn . show

charAsString :: Char -> String
charAsString '\b' = "\\\\b"
charAsString '\t' = "\\\\b"
charAsString '\n' = "\\\\b"
charAsString '\f' = "\\\\b"
charAsString '\r' = "\\\\b"
charAsString ' '  = "'  '"
charAsString c    = [c]

printChars :: String -> String
printChars [] = []
printChars xs = printChars' xs 1
    where printChars' [] _ = "|"
          printChars' (x:xs) n = "|  " ++ charAsString x ++ end ++ printChars' xs (n+1)
            where end = if 0 == n `mod` 8 then "\n" else drop (length $ charAsString x) "     "

compiler :: [String] -> IO ()
compiler [ ]         = displayDiagnostics $ report handler (failure "Too few arguments." (position 0 0))
compiler (x1:x2:xs)  = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    input <- readFile $ fileName ++ ".mini"
    putStrLn $ printChars input