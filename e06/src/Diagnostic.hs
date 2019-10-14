module Diagnostic
    ( Diagnostic(..)
    , Position(..)
    , textOf
    , posOf
    , crossRef
    , rowOf
    , colOf
    , coordString
    , failure
    , warning
    , position
    ) where

import Position

data Diagnostic = Failure { text :: String
                          , pos :: Position
                          }
                | Warning { text :: String
                          , pos :: Position
                          } 

failure :: String -> Position -> Diagnostic
failure text pos = Failure { text = text, pos = pos }

warning :: String -> Position -> Diagnostic
warning text pos = Warning { text = text, pos = pos }

textOf :: Diagnostic -> String
textOf Failure { text = text } = text
textOf Warning { text = text } = text

posOf :: Diagnostic -> Position
posOf Failure { pos = pos } = pos
posOf Warning { pos = pos } = pos

crossRef :: Diagnostic -> String
crossRef Failure {} = "Failure: unimplemented"
crossRef Warning {} = "Warning: unimplemented"

instance Show Diagnostic where
    show Failure { text = text
               , pos = pos
               } = "FAILURE " ++ coordString pos ++ ": " ++ text
    show Warning { text = text
               , pos = pos
               } = "WARNING " ++ coordString pos ++ ": " ++ text