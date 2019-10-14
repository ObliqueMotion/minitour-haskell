module Token
    ( Token(..)
    , isDiagnostic
    , toDiagnostic
    ) where

import Diagnostic

isDiagnostic :: Token -> Bool
isDiagnostic t = case t of
                 Error _ -> True
                 _ -> False

toDiagnostic :: Token -> Diagnostic
toDiagnostic t = case t of
                 Error d -> d
                 _ -> error "You done messed up."


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