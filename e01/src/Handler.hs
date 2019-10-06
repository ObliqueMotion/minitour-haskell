module Handler
    ( Handler(..)
    , report
    , hasFailures
    , hasWarnings
    ) where

import Diagnostic
import Position
import Data.List

data Handler = Handler { failures :: Int
                       , warnings :: Int
                       , diagnostics :: [Diagnostic]
                       }

report :: Handler -> Diagnostic -> Handler
report Handler { failures = failures
               , warnings = warnings
               , diagnostics = diagnostics
               }
       failure @ Failure {} = Handler { failures = failures + 1
                                      , warnings = warnings
                                      , diagnostics = (failure : diagnostics)
                                      }
report Handler { failures = failures
               , warnings = warnings
               , diagnostics = diagnostics
               }
       warning @ Warning {} = Handler { failures = failures
                                      , warnings = warnings + 1
                                      , diagnostics = (warning : diagnostics)
                                      }
                            
hasFailures :: Handler -> Bool
hasFailures Handler { failures = failures } = failures > 0

hasWarnings :: Handler -> Bool
hasWarnings Handler { warnings = warnings } = warnings > 0

instance Show Handler where
    show Handler { failures = failures
                 , warnings = warnings
                 , diagnostics = diagnostics
                 } =  "\nHandler: (" 
                   ++ show failures
                   ++ " Failures, "
                   ++ show warnings
                   ++ " Warnings)\n"
                   ++ (concat . map show) diagnostics
                   ++ "\n"