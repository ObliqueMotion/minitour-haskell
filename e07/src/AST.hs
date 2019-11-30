module AST
    ( Expr(..)
    , Statement(..)
    , Type(..)
    ) where

import Position

data Type = Int'
          | Boolean

data Statement = Empty
               | Assign String Expr
               | Block [Statement]
               | While Expr Statement
               | If Expr Statement Statement
               | Print Expr
               | VarDecl Type [Expr]


data Expr = IntLit Int
          | Lt     Expr Expr
          | Gt     Expr Expr
          | Gte    Expr Expr
          | Lte    Expr Expr
          | Neq    Expr Expr
          | Eql    Expr Expr
          | BOr    Expr Expr
          | BAnd   Expr Expr
          | BXor   Expr Expr
          | LOr    Expr Expr
          | LAnd   Expr Expr
          | LNot   Expr
          | BNot   Expr
          | UPlus  Expr
          | UMinus Expr
          | BoolLit Bool
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Identifier String
          deriving(Show, Eq)