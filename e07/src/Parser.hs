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

parse :: [Token] -> [Statement]
parse = fst . head . apply (parseStatements Token.EndInput)

parseToken :: Parser Token
parseToken = C (\inp -> case inp of
                             []     -> []
                             (t:ts) -> [(t, ts)])

require :: TokenConstructor -> Parser ()
require c = let lhs = c start
            in do rhs <- parseToken
                  if tokEq lhs rhs
                     then return ()
                     else Combinator.fail

parseStatements :: TokenConstructor -> Parser [Statement]
parseStatements stopCondition = do ss <- many parseStatement
                                   require stopCondition
                                   return ss

parseStatement :: Parser Statement
parseStatement = do t <- parseToken
                    case t of
                         Token.Semicolon _      -> return AST.Empty
                         Token.Identifier val _ -> parseAssignment val
                         Token.OpenBrace _      -> parseBlock
                         Token.While _          -> parseWhile
                         Token.If _             -> parseIf
                         Token.Print _          -> parsePrint
                         otherwise              -> Combinator.fail
                <|> parseVarDecl

parseAssignment :: String -> Parser Statement
parseAssignment lhs = do require Token.Assignment
                         rhs <- parseExpr
                         require Token.Semicolon
                         return $ AST.Assign lhs rhs

parseBlock :: Parser Statement
parseBlock = do ss <- parseStatements Token.CloseBrace 
                return $ Block ss

parseWhile :: Parser Statement
parseWhile = do test <- parseTest
                body  <- parseStatement
                return $ AST.While test body

parseIf :: Parser Statement
parseIf = do test    <- parseTest
             ifTrue  <- parseStatement
             ifFalse <- do require Token.Else
                           parseStatement
                       <|> return AST.Empty
             return $ AST.If test ifTrue ifFalse

parseTest :: Parser Expr
parseTest = do require Token.OpenParen
               e <- parseExpr
               require Token.CloseParen
               return e

parsePrint :: Parser Statement
parsePrint = do e <- parseExpr
                require Token.Semicolon
                return $ AST.Print e

parseVarDecl :: Parser Statement
parseVarDecl = do t    <- parseType
                  vars <- parseIds
                  require Token.Semicolon
                  return $ VarDecl t vars

parseType :: Parser Type
parseType = do t <- parseToken
               case t of
                    Token.Int' _    -> return AST.Int'
                    Token.Boolean _ -> return AST.Boolean
                    otherwise       -> Combinator.fail

parseId :: Parser Expr
parseId = do t <- parseToken
             case t of 
                  Token.Identifier s _ -> return $ AST.Identifier s
                  otherwise            -> return $ AST.Identifier "toast"

parseIds :: Parser [Expr]
parseIds = do ids <- many (do id <- parseId
                              require Token.Comma
                              return id)
              id  <- parseId
              return (ids ++ [id])

parseExpr :: Parser Expr
parseExpr = parseLOr

parseLOr :: Parser Expr
parseLOr = do lhs <- parseLAnd
              op  <- parseToken
              case op of
                   Token.LogicalOr _ -> do rhs <- parseLAnd
                                           return $ AST.LOr lhs rhs
                   otherwise         -> Combinator.fail
          <|> parseLAnd

parseLAnd :: Parser Expr
parseLAnd = do lhs <- parseBOr
               op  <- parseToken
               case op of
                    Token.LogicalAnd _ -> do rhs <- parseBOr
                                             return $ AST.LAnd lhs rhs
                    otherwise          -> Combinator.fail
           <|> parseBOr

parseBOr :: Parser Expr
parseBOr = do lhs <- parseBXor
              op  <- parseToken
              case op of
                   Token.BitOr _ -> do rhs <- parseBXor
                                       return $ AST.BOr lhs rhs
                   otherwise     -> Combinator.fail
          <|> parseBXor

parseBXor :: Parser Expr
parseBXor = do lhs <- parseBAnd
               op  <- parseToken
               case op of
                    Token.BitXor _ -> do rhs <- parseBAnd
                                         return $ AST.BXor lhs rhs
                    otherwise      -> Combinator.fail
           <|> parseBAnd

parseBAnd :: Parser Expr
parseBAnd = do lhs <- parseEql
               op  <- parseToken
               case op of
                    Token.BitAnd _ -> do rhs <- parseEql
                                         return $ AST.BAnd lhs rhs
                    otherwise      -> Combinator.fail
           <|> parseEql

parseEql :: Parser Expr
parseEql = do lhs <- parseRel
              op  <- parseToken
              case op of
                   Token.Eq _    -> do rhs <- parseRel
                                       return $ AST.Eql lhs rhs
                   Token.NotEq _ -> do rhs <- parseRel
                                       return $ AST.Neq lhs rhs
                   otherwise     -> Combinator.fail
          <|> parseRel

parseRel :: Parser Expr
parseRel = do lhs <- parseAdd
              op  <- parseToken
              case op of
                   Token.LessThan _    -> do rhs <- parseAdd
                                             return $ AST.Lt lhs rhs
                   Token.LessOrEq _    -> do rhs <- parseAdd
                                             return $ AST.Lte lhs rhs
                   Token.GreaterThan _ -> do rhs <- parseAdd
                                             return $ AST.Gt lhs rhs
                   Token.GreaterOrEq _ -> do rhs <- parseAdd
                                             return $ AST.Gte lhs rhs
                   otherwise           -> Combinator.fail
          <|> parseAdd
                                            

parseAdd :: Parser Expr
parseAdd  = do e <- parseMul
               parseAdd' e
           <|> parseMul

parseAdd' :: Expr -> Parser Expr
parseAdd' e = do t  <- parseToken
                 case t of
                      Token.Add _ -> do e' <- parseMul
                                        parseAdd' $ AST.Add e e'
                      Token.Sub _ -> do e' <- parseMul
                                        parseAdd' $ AST.Sub e e'
                      otherwise   -> Combinator.fail
             <|> return e

parseMul :: Parser Expr
parseMul  = do e <- parseUnary
               parseMul' e
           <|> parseUnary

parseMul' :: Expr -> Parser Expr
parseMul' e = do t  <- parseToken
                 case t of
                      Token.Mul _ -> do e' <- parseUnary
                                        parseMul' $ AST.Mul e e'
                      Token.Div _ -> do e' <- parseUnary
                                        parseMul' $ AST.Div e e'
                      otherwise   -> Combinator.fail
             <|> return e

parseUnary :: Parser Expr
parseUnary  = do t <- parseToken
                 e <- parsePrimary
                 case t of 
                      Token.Add   _ -> return $ AST.UPlus  e
                      Token.Sub   _ -> return $ AST.UMinus e
                      Token.Not   _ -> return $ AST.LNot   e
                      Token.Tilde _ -> return $ AST.BNot   e
                      otherwise     -> Combinator.fail
             <|> parsePrimary

parsePrimary :: Parser Expr
parsePrimary = do t <- parseToken
                  case t of
                       Token.IntLit val _     -> return $ AST.IntLit val
                       Token.True' _          -> return $ AST.BoolLit True
                       Token.False' _         -> return $ AST.BoolLit False
                       Token.Identifier val _ -> return $ AST.Identifier val
                       Token.OpenParen _      -> do e <- parseExpr
                                                    require Token.CloseParen
                                                    return e
                       otherwise              -> Combinator.fail
