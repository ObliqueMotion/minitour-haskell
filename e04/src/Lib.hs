module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position
import Data.Char (ord)
import System.Directory (doesFileExist)

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

displayDiagnostics :: Handler -> IO ()
displayDiagnostics = putStrLn . show

charAsString :: Char -> String
charAsString '\b' = "\\b"
charAsString '\t' = "\\t"
charAsString '\n' = "\\n"
charAsString '\f' = "\\f"
charAsString '\r' = "\\r"
charAsString ' '  = "' '"
charAsString c    = [c]

printChars :: String -> String
printChars [] = []
printChars xs = printChars' xs 1
    where printChars' [] _ = "|"
          printChars' (x:xs) n = "|  " ++ charAsString x ++ end ++ printChars' xs (n+1)
            where end = if 0 == n `mod` 8 then "\n" else drop (length $ charAsString x) "     "

compiler :: [String] -> IO ()
compiler [ ]        = displayDiagnostics $ report handler (failure "Too few arguments."  (position 0 0))
compiler (x1:x2:xs) = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    let path = fileName ++ ".mini"
    fileExists <- doesFileExist path
    if fileExists then do
        input <- readFile path
        putStrLn $ printChars input
    else
        putStrLn $ show $ report handler (failure ("File (" ++ path ++ ") does not exist.") (position 0 0))