{-# LANGUAGE RecordWildCards #-}

import Data.Either (partitionEithers)
import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import AST
import Token
import Lexer
import Parser

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

tokensFrom :: String -> [Token]
tokensFrom = fst . partitionEithers . lexer

exprFrom :: [Token] -> Expr
exprFrom = fst . parseExpr

specs :: Spec
specs = describe "f" $ for_ cases test
    where test Case{..} = it description $ let tokens = tokensFrom expression 
                                           in (tokens, exprFrom tokens) `shouldBe` (expectedTokens, expectedExpr)

data Case =  Case { description :: String
                       , expression :: String
                       , expectedTokens :: [Token]
                       , expectedExpr :: Expr
                       }

cases :: [Case]
cases = [ Case  { description = "Case:  1 +  2"
                , expression = "1 + 2"
                , expectedTokens = [IntLit 1, Token.Add, IntLit 2]
                , expectedExpr = AST.Add (Lit 1) (Lit 2)
                }
        , Case  { description = "Case:  1 -  2"
                , expression = "1 - 2"
                , expectedTokens = [IntLit 1, Token.Sub, IntLit 2]
                , expectedExpr = AST.Sub (Lit 1) (Lit 2)
                }
        , Case  { description = "Case:  1 + -2"
                , expression = "1 + -2"
                , expectedTokens = [IntLit 1, Token.Add, Token.Sub, IntLit 2]
                , expectedExpr = AST.Add (Lit 1) (AST.UMinus (Lit 2))
                }        
        , Case  { description = "Case: -1 - +2"
                , expression = "-1 - +2"
                , expectedTokens = [Token.Sub, IntLit 1, Token.Sub, Token.Add, IntLit 2]
                , expectedExpr = AST.Sub (AST.UMinus (Lit 1)) (AST.UPlus (Lit 2))
                } 
        , Case  { description = "Case:  1 *  2"
                , expression = "1 * 2"
                , expectedTokens = [IntLit 1, Token.Mul, IntLit 2]
                , expectedExpr = AST.Mul (Lit 1) (Lit 2)
                }
        , Case  { description = "Case:  1 /  2"
                , expression = "1 / 2"
                , expectedTokens = [IntLit 1, Token.Div, IntLit 2]
                , expectedExpr = AST.Div (Lit 1) (Lit 2)
                }
        , Case  { description = "Case:  1 * -2"
                , expression = "1 * -2"
                , expectedTokens = [IntLit 1, Token.Mul, Token.Sub, IntLit 2]
                , expectedExpr = AST.Mul (Lit 1) (AST.UMinus (Lit 2))
                }        
        , Case  { description = "Case: -1 / +2"
                , expression = "-1 / +2"
                , expectedTokens = [Token.Sub, IntLit 1, Token.Div, Token.Add, IntLit 2]
                , expectedExpr = AST.Div (AST.UMinus (Lit 1)) (AST.UPlus (Lit 2))
                } 
        , Case  { description = "Case: 1 + 2 + 3 + 4 + 5"
                , expression = "1 + 2 + 3 + 4 + 5"
                , expectedTokens = [IntLit 1, Token.Add, IntLit 2, Token.Add, IntLit 3, Token.Add, IntLit 4, Token.Add, IntLit 5]
                , expectedExpr = AST.Add (Lit 5) (AST.Add (Lit 4) (AST.Add (Lit 3) (AST.Add (Lit 1) (Lit 2))))
                }
        , Case  { description = "Case: 1 + 2 * 3"
                , expression = "1 + 2 * 3"
                , expectedTokens = [IntLit 1, Token.Add, IntLit 2, Token.Mul, IntLit 3]
                , expectedExpr = AST.Add (Lit 1) (AST.Mul (Lit 2) (Lit 3))
                }
        , Case  { description = "Case: 1 * 2 + 3"
                , expression = "1 * 2 + 3"
                , expectedTokens = [IntLit 1, Token.Mul, IntLit 2, Token.Add, IntLit 3]
                , expectedExpr = AST.Add (AST.Mul (Lit 1) (Lit 2)) (Lit 3)
                }
        ]