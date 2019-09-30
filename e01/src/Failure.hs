module Failure
    ( Failure(..)
    ) where

import Position
import Diagnostic

data Failure = Failure { text :: String
                       , position :: Position
                       }


instance Diagnostic Failure where
    textOf Failure { text = text } = text
    positionOf Failure { position = position } = position
    crossRef Failure {} = "unimplemented"