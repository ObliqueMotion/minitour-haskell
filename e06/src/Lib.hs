module Lib
    ( compiler
    ) where

import Lexer
import Handler
import Token
import Diagnostic
import Position
import Data.Char (ord)
import System.Directory (doesFileExist)
import Data.Either (partitionEithers)

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

lineNumber :: Int -> String
lineNumber x = (show x) ++ ":    "

numberedLines :: String -> String
numberedLines s = concat $ zipWith (++) lineNumbers lineStrings
    where lineNumbers = map lineNumber [1..]
          lineStrings = map (++ "\n") $ lines s


compiler :: [String] -> IO ()
compiler [ ]        = putStrLn $ show $ report handler (failure "Too few arguments."  (position 0 0))
compiler (x1:x2:xs) = putStrLn $ show $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    let path = fileName ++ ".mini"
    fileExists <- doesFileExist path
    if fileExists then do
        input <- readFile path
        putStrLn input
        let (tokens, diagnostics) = partitionEithers $ lexer input
        if null diagnostics then mapM_ print tokens
                            else print $ foldl report handler diagnostics
    else
        putStrLn $ show $ report handler (failure ("File (" ++ path ++ ") does not exist.") (position 0 0))