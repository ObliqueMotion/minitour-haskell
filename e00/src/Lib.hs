module Lib
    ( compiler
    ) where

compiler :: [String] -> IO ()
compiler [ ]  = putStrLn "This program requires exactly one argument"
compiler [x]  = putStrLn $ "Ok, we should look for an input called " ++ x
compiler args = mapM_ putStrLn ("Too many arguments were supplied:":args)
