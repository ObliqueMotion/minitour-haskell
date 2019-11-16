module Parser
    ( parse
    ) where

import AST
import Token
import Combinator
import Control.Applicative

type Parser = Combinator [Token]


token :: Parser Token
token = C (\inp -> case inp of
                   []     -> []
                   (t:ts) -> [(t, ts)])

parse :: [Token] -> Expr
parse = fst . head . apply expr

expr :: Parser Expr
expr = add

add :: Parser Expr
add  = do e <- mul
          add' e
      <|> mul

add' :: Expr -> Parser Expr
add' e = do t  <- token
            e' <- mul
            case t of
                Token.Add _ -> add' (AST.Add e e')
                Token.Sub _ -> add' (AST.Sub e e')
                otherwise   -> Combinator.fail
        <|> return e

mul :: Parser Expr
mul  = do e <- unary
          mul' e
      <|> unary

mul' :: Expr -> Parser Expr
mul' e = do t  <- token
            e' <- unary
            case t of
                Token.Mul _ -> mul' (AST.Mul e e')
                Token.Div _ -> mul' (AST.Div e e')
                otherwise   -> Combinator.fail
        <|> return e

unary :: Parser Expr
unary  = do t <- token
            e <- primary
            case t of 
               Token.Add   _ -> return (AST.UPlus  e)
               Token.Sub   _ -> return (AST.UMinus e)
               Token.Not   _ -> return (AST.LNot   e)
               Token.Tilde _ -> return (AST.BNot   e)
               otherwise     -> Combinator.fail
        <|> primary

primary :: Parser Expr
primary = do t <- token
             case t of
                Token.IntLit val _ -> return (Lit val)
                otherwise          -> Combinator.fail
