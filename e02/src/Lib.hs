module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

compiler :: [String] -> IO ()
compiler [ ]         = putStrLn $ show $ report handler (failure "Too few arguments." (position 0 0))
compiler (x1:x2:xs)  = putStrLn $ show $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    input <- (readFile (fileName ++ ".mini"))
    putStrLn input 