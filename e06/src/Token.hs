module Token
    ( Token(..)
    ) where

import Diagnostic

data Token = EndInput
           | Error Diagnostic
           | Identifier String
           | IntLit Int
           | Eq
           | NotEq
           | LessOrEq
           | GreaterOrEq
           | LogicalAnd
           | LogicalOr
           | If
           | Else
           | While
           | Print
           | Int'
           | Boolean
           | True'
           | False'
           | OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | Semicolon
           | Comma
           | Assignment
           | Not
           | LessThan
           | GreaterThan
           | BitAnd
           | BitOr
           | BitXor
           | Tilde
           | Add
           | Sub
           | Mul
           | Div
           deriving Show