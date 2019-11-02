module Parser
    ( parseExpr
    ) where

import AST
import Token

parseExpr :: [Token] -> (Expr, [Token])
parseExpr = parseAdd

parseAdd :: [Token] -> (Expr, [Token])
parseAdd = parseAdd' . parseMult
              where parseAdd' (e, (Token.Add:ts)) = let (e', tss) = parseMult ts in (AST.Add e e', tss)
                    parseAdd' (e, (Token.Sub:ts)) = let (e', tss) = parseMult ts in (AST.Sub e e', tss)
                    parseAdd' x = x

parseMult :: [Token] -> (Expr, [Token])
parseMult = parseMult' . parseUnary
              where parseMult' (e, (Token.Mul:ts)) = let (e', tss) = parseUnary ts in (AST.Mul e e', tss)
                    parseMult' (e, (Token.Div:ts)) = let (e', tss) = parseUnary ts in (AST.Div e e', tss)
                    parseMult' x = x

parseUnary :: [Token] -> (Expr, [Token])
parseUnary (Token.Add:ts)   = let (e, tss) = parseUnary ts in (AST.UPlus  e, tss)
parseUnary (Token.Sub:ts)   = let (e, tss) = parseUnary ts in (AST.UMinus e, tss)
parseUnary (Token.Not:ts)   = let (e, tss) = parseUnary ts in (AST.LNot   e, tss)
parseUnary (Token.Tilde:ts) = let (e, tss) = parseUnary ts in (AST.BNot   e, tss)
parseUnary x = parsePrimary x

parsePrimary :: [Token] -> (Expr, [Token])
parsePrimary (Token.IntLit val:ts) = (Lit val, ts)
parsePrimary ts = (Lit 5, ts)