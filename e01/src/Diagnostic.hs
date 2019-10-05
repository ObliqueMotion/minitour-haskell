module Diagnostic
    ( Diagnostic(..)
    , Position(..)
    , textOf
    , positionOf
    , crossRef
    , rowOf
    , colOf
    , coordString
    ) where

import Position

data Diagnostic = Failure { text :: String
                          , position :: Position
                          }
                | Warning { text :: String
                          , position :: Position
                          } 

textOf :: Diagnostic -> String
textOf Failure { text = text } = text
textOf Warning { text = text } = text

positionOf :: Diagnostic -> Position
positionOf Failure { position = position } = position
positionOf Warning { position = position } = position

crossRef :: Diagnostic -> String
crossRef Failure {} = "Failure: unimplemented"
crossRef Warning {} = "Warning: unimplemented"

instance Show Diagnostic where
    show Failure { text = text
               , position = position
               } = "FAILURE " ++ coordString position ++ ": " ++ text
    show Warning { text = text
               , position = position
               } = "WARNING " ++ coordString position ++ ": " ++ text