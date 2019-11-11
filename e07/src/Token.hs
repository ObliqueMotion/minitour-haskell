module Token
    ( Token(..)
    ) where

import Diagnostic

data Token = EndInput Position
           | Identifier String Position
           | IntLit Int Position
           | Eq Position
           | NotEq Position
           | LessOrEq Position
           | GreaterOrEq Position
           | LogicalAnd Position
           | LogicalOr Position
           | If Position
           | Else Position
           | While Position
           | Print Position
           | Int' Position
           | Boolean Position
           | True' Position
           | False' Position
           | OpenParen Position
           | CloseParen Position
           | OpenBrace Position
           | CloseBrace Position
           | Semicolon Position
           | Comma Position
           | Assignment Position
           | Not Position
           | LessThan Position
           | GreaterThan Position
           | BitAnd Position
           | BitOr Position
           | BitXor Position
           | Tilde Position
           | Add Position
           | Sub Position
           | Mul Position
           | Div Position
           deriving (Show, Eq)