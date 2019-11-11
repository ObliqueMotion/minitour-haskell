module Parser
    ( parseExpr
    ) where

import AST
import Token

parseExpr :: [Token] -> (Expr, [Token])
parseExpr = parseAdd

parseAdd :: [Token] -> (Expr, [Token])
parseAdd = parseAdd' . parseMul
              where parseAdd' (e, (Token.Add _:ts)) = let (e', tss) = parseMul ts in parseAdd' ((AST.Add e e'), tss)
                    parseAdd' (e, (Token.Sub _:ts)) = let (e', tss) = parseMul ts in parseAdd' ((AST.Sub e e'), tss)
                    parseAdd' x = x

parseMul :: [Token] -> (Expr, [Token])
parseMul = parseMul' . parseUnary
              where parseMul' (e, (Token.Mul _:ts)) = let (e', tss) = parseUnary ts in parseMul' ((AST.Mul e e'), tss)
                    parseMul' (e, (Token.Div _:ts)) = let (e', tss) = parseUnary ts in parseMul' ((AST.Div e e'), tss)
                    parseMul' x = x

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (Token.Add _:ts)   = let (e, tss) = parsePrimary ts in (AST.UPlus  e, tss)
parseUnary (Token.Sub _:ts)   = let (e, tss) = parsePrimary ts in (AST.UMinus e, tss)
parseUnary (Token.Not _:ts)   = let (e, tss) = parsePrimary ts in (AST.LNot   e, tss)
parseUnary (Token.Tilde _:ts) = let (e, tss) = parsePrimary ts in (AST.BNot   e, tss)
parseUnary ts = parsePrimary ts

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (Token.IntLit val _:ts) = (Lit val, ts)