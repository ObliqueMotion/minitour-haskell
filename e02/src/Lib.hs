module Lib
    ( compiler
    ) where

import Handler
import Diagnostic
import Position
import Control.Exception (try, Exception)

handler :: Handler
handler = Handler { failures = 0, warnings = 0, diagnostics = []}

displayDiagnostics :: Handler -> IO ()
displayDiagnostics = putStrLn . show

compiler :: [String] -> IO ()
compiler [ ]        = displayDiagnostics $ report handler (failure "Too few arguments."  (position 0 0))
compiler (x1:x2:xs) = displayDiagnostics $ report handler (failure "Too many arguments." (position 0 0))
compiler [fileName] = do 
    result <- try $ readFile (fileName ++ ".mini")
    case result of 
        Left errorMessage -> putStrLn errorMessage --displayDiagnostics $ report handler (failure errorMessage (position 0 0))
        Right fileContent -> putStrLn fileContent