module AST
    ( Expr(..)
    ) where

data Expr = Lit Int
    | LNot   Expr
    | BNot   Expr
    | UPlus  Expr
    | UMinus Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving(Show, Eq)