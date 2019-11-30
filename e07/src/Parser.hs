module Parser
    ( parse
    ) where

import AST
import Token
import Position
import Combinator
import Control.Applicative

type Parser = Combinator [Token]
type TokenConstructor = (Position -> Token)


token :: Parser Token
token = C (\inp -> case inp of
                        []     -> []
                        (t:ts) -> [(t, ts)])

require :: TokenConstructor -> Parser ()
require c = let lhs = c start
            in do rhs <- token
                  if tokEq lhs rhs
                  then return ()
                  else Combinator.fail

parse :: [Token] -> Expr
parse = fst . head . apply expr

statements :: TokenConstructor -> Parser [Statement]
statements stopCondition = do ss <- many statement
                              require stopCondition
                              return ss

statement :: Parser Statement
statement = do t <- token
               case t of
                    Token.Semicolon _      -> return AST.Empty
                    Token.Identifier val _ -> assign val
                    Token.OpenBrace _      -> block
                    Token.While _          -> while
                    Token.If _             -> parseIf
                    otherwise -> Combinator.fail

assign :: String -> Parser Statement
assign lhs = do require Token.Assignment
                rhs <- expr
                require Token.Semicolon
                return $ AST.Assign lhs rhs

block :: Parser Statement
block = do ss <- statements Token.CloseBrace 
           return $ Block ss

while :: Parser Statement
while = do test' <- test
           body  <- statement
           return $ AST.While test' body

parseIf :: Parser Statement
parseIf = do test'   <- test
             ifTrue  <- statement
             ifFalse <- do require Token.Else
                           statement
                       <|> return AST.Empty
             return $ AST.If test' ifTrue ifFalse

test :: Parser Expr
test = do require Token.OpenParen
          e <- expr
          require Token.CloseParen
          return e

expr :: Parser Expr
expr = add

add :: Parser Expr
add  = do e <- mul
          add' e
      <|> mul

add' :: Expr -> Parser Expr
add' e = do t  <- token
            case t of
                 Token.Add _ -> do e' <- mul
                                   add' $ AST.Add e e'
                 Token.Sub _ -> do e' <- mul
                                   add' $ AST.Sub e e'
                 otherwise   -> Combinator.fail
        <|> return e

mul :: Parser Expr
mul  = do e <- unary
          mul' e
      <|> unary

mul' :: Expr -> Parser Expr
mul' e = do t  <- token
            case t of
                 Token.Mul _ -> do e' <- unary
                                   mul' $ AST.Mul e e'
                 Token.Div _ -> do e' <- unary
                                   mul' $ AST.Div e e'
                 otherwise   -> Combinator.fail
        <|> return e

unary :: Parser Expr
unary  = do t <- token
            e <- primary
            case t of 
                 Token.Add   _ -> return $ AST.UPlus  e
                 Token.Sub   _ -> return $ AST.UMinus e
                 Token.Not   _ -> return $ AST.LNot   e
                 Token.Tilde _ -> return $ AST.BNot   e
                 otherwise     -> Combinator.fail
        <|> primary

primary :: Parser Expr
primary = do t <- token
             case t of
                  Token.IntLit val _     -> return $ AST.IntLit val
                  Token.True' _          -> return $ AST.BoolLit True
                  Token.False' _         -> return $ AST.BoolLit False
                  Token.Identifier val _ -> return $ AST.Identifier val
                  Token.OpenParen _      -> do e <- expr
                                               require Token.CloseParen
                                               return e
                  otherwise              -> Combinator.fail
