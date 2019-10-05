module Handler
    ( Handler(..)
    , report
    , hasFailures
    , hasWarnings
    ) where

import Diagnostic
import Position

data Handler = Handler { failures :: Int
                       , warnings :: Int
                       }

report :: Handler -> Diagnostic -> Handler
report Handler { failures = failures
               , warnings = warnings
               }
       Failure {} = Handler { failures = failures + 1
                            , warnings = warnings
                            }
report Handler { failures = failures
               , warnings = warnings
               }
       Warning {} = Handler { failures = failures
                            , warnings = warnings + 1
                            }
                            
hasFailures :: Handler -> Bool
hasFailures Handler { failures = failures } = failures > 0

hasWarnings :: Handler -> Bool
hasWarnings Handler { warnings = warnings } = warnings > 0

instance Show Handler where
    show Handler { failures = failures
                 , warnings = warnings
                 } = "Handler: " ++ show failures ++ " failures, " ++ show warnings ++ " warnings"