module Token
    ( Token(..)
    , tokEq
    ) where

import Diagnostic
import Position

tokEq :: Token -> Token -> Bool
tokEq (EndInput _) (EndInput _) = True
tokEq (Identifier s1 _) (Identifier s2 _) = s1 == s2
tokEq (IntLit x1 _) (IntLit x2 _) = x1 == x2
tokEq (Eq _) (Eq _) = True
tokEq (NotEq _) (NotEq _) = True
tokEq (LessOrEq _)(LessOrEq _) = True
tokEq (GreaterOrEq _)(GreaterOrEq _) = True
tokEq (LogicalAnd _) (LogicalAnd _) = True
tokEq (LogicalOr _)(LogicalOr _) = True
tokEq (If _) (If _) = True
tokEq (Else _) (Else _) = True
tokEq (While _) (While _) = True
tokEq (Print _) (Print _) = True
tokEq (Int' _) (Int' _) = True
tokEq (Boolean _) (Boolean _) = True
tokEq (True' _) (True' _) = True
tokEq (False' _) (False' _) = True
tokEq (OpenParen _) (OpenParen _) = True
tokEq (CloseParen _) (CloseParen _) = True
tokEq (OpenBrace _) (OpenBrace _) = True
tokEq (CloseBrace _) (CloseBrace _) = True
tokEq (Semicolon _) (Semicolon _) = True
tokEq (Comma _) (Comma _) = True
tokEq (Assignment _) (Assignment _) = True
tokEq (Not _) (Not _) = True
tokEq (LessThan _) (LessThan _) = True
tokEq (GreaterThan _) (GreaterThan _) = True
tokEq (BitAnd _) (BitAnd _) = True
tokEq (BitOr _) (BitOr _) = True
tokEq (BitXor _) (BitXor _) = True
tokEq (Tilde _) (Tilde _) = True
tokEq (Add _) (Add _) = True
tokEq (Sub _) (Sub _) = True
tokEq (Mul _) (Mul _) = True
tokEq (Div _) (Div _) = True
tokEq _ _ = False

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