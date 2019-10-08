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

asciiValue :: Char -> String
asciiValue = show . ord

printIntegers :: String -> String
printIntegers [] = []
printIntegers xs = printIntegers' xs 1
    where printIntegers' [] _ = []
          printIntegers' (x:xs) n = "|  " ++ asciiValue x ++ end ++ printIntegers' xs (n+1)
            where end = if 0 == n `mod` 8 then "\n" else drop (length $ asciiValue x) "        "

compiler :: [String] -> IO ()
compiler [ ]        = displayDiagnostics $ report handler (failure "Too few arguments."  (position 0 0))
compiler (x1:x2:xs) = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    let path = fileName ++ ".mini"
    fileExists <- doesFileExist path
    if fileExists then do
        input <- readFile path
        putStrLn $ printIntegers input
    else
        putStrLn $ show $ report handler (failure ("File (" ++ path ++ ") does not exist.") (position 0 0))