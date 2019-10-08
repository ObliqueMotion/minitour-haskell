module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position
import System.Directory (doesFileExist)


handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

displayDiagnostics :: Handler -> IO ()
displayDiagnostics = putStrLn . show

compiler :: [String] -> IO ()
compiler [ ]        = displayDiagnostics $ report handler (failure "Too few arguments."  (position 0 0))
compiler (x1:x2:xs) = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    let path = fileName ++ ".mini"
    fileExists <- doesFileExist path
    if fileExists then do
        input <- readFile path
        putStrLn input
    else
        putStrLn $ show $ report handler (failure ("File (" ++ path ++ ") does not exist.") (position 0 0))