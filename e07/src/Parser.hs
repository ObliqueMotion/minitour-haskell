module Parser
    ( parseExpr
    ) where

import AST
import Token

parseExpr :: [Token] -> (Expr, [Token])
parseExpr = parseAdd

parseAdd :: [Token] -> (Expr, [Token])
parseAdd = parseAdd' . parseMul
              where parseAdd' (e, (Token.Add:ts)) = let (e', tss) = parseMul ts in parseAdd' ((AST.Add e e'), tss)
                    parseAdd' (e, (Token.Sub:ts)) = let (e', tss) = parseMul ts in parseAdd' ((AST.Sub e e'), tss)
                    parseAdd' x = x

parseMul :: [Token] -> (Expr, [Token])
parseMul = parseMul' . parseUnary
              where parseMul' (e, (Token.Mul:ts)) = let (e', tss) = parseUnary ts in parseMul' ((AST.Mul e e'), tss)
                    parseMul' (e, (Token.Div:ts)) = let (e', tss) = parseUnary ts in parseMul' ((AST.Div e e'), tss)
                    parseMul' x = x

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (Token.Add:ts)   = let (e, tss) = parsePrimary ts in (AST.UPlus  e, tss)
parseUnary (Token.Sub:ts)   = let (e, tss) = parsePrimary ts in (AST.UMinus e, tss)
parseUnary (Token.Not:ts)   = let (e, tss) = parsePrimary ts in (AST.LNot   e, tss)
parseUnary (Token.Tilde:ts) = let (e, tss) = parsePrimary ts in (AST.BNot   e, tss)
parseUnary ts = parsePrimary ts

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (Token.IntLit val:ts) = (Lit val, ts)