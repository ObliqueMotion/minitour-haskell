module Warning
    ( Warning(..)
    ) where

import Position
import Diagnostic

data Warning = Warning { text :: String
                       , position :: Position
                       }


instance Diagnostic Warning where
    textOf Warning { text = text } = text
    positionOf Warning { position = position } = position
    crossRef Warning {} = "unimplemented"