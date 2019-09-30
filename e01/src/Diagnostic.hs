module Diagnostic
    ( Diagnostic(..)
    ) where

import Position

class Diagnostic a where
    textOf :: a -> String
    positionOf :: a -> Position
    crossRef :: a -> String