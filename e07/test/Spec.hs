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
import Position
import Data.Char

main :: IO ()
main = mapM_ (hspecWith defaultConfig {configFastFail = False}) [specs]

tokensFrom :: String -> [Token]
tokensFrom = fst . Lexer.lex

exprFrom :: [Token] -> Expr
exprFrom = fst . parseExpr

specs :: Spec
specs = describe "Lexing and Parsing" $ for_ parseCases test
    where test ParseCase{..} = it parseDescription $ let tokens = tokensFrom parseInput 
                                           in (tokens, exprFrom tokens) `shouldBe` (parseExpectedTokens, parseExpectedExpr)

withPositions :: String -> [(Position -> Token)] -> [Token]
withPositions parseInput constructors = zipWith ($) constructors positions
    where positions = map fst 
                    $ filter (\(_,c) -> not $ isSpace c) 
                    $ zip (scanl (+>) start parseInput) parseInput

data Case = LexCase   { lexDescription :: String 
                      , lexInput :: String 
                      , lexExpectedTokens :: [Token]
                      }
          | ParseCase { parseDescription :: String
                      , parseInput :: String
                      , parseExpectedTokens :: [Token]
                      , parseExpectedExpr :: Expr
                      }

parseCases :: [Case]
parseCases = [ ParseCase  { parseDescription = "ParseCase:  1 +  2"
                          , parseInput = "1 + 2"
                          , parseExpectedTokens = withPositions "1 + 2" [IntLit 1, Token.Add, IntLit 2]
                          , parseExpectedExpr = AST.Add (Lit 1) (Lit 2)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 -  2"
                          , parseInput = "1 - 2"
                          , parseExpectedTokens = withPositions "1 - 2" [IntLit 1, Token.Sub, IntLit 2]
                          , parseExpectedExpr = AST.Sub (Lit 1) (Lit 2)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 + -2"
                          , parseInput = "1 + -2"
                          , parseExpectedTokens = withPositions "1 + -2" [IntLit 1, Token.Add, Token.Sub, IntLit 2]
                          , parseExpectedExpr = AST.Add (Lit 1) (AST.UMinus (Lit 2))
                          }        
             , ParseCase  { parseDescription = "ParseCase: -1 - +2"
                          , parseInput = "-1 - +2"
                          , parseExpectedTokens = withPositions "-1 - +2" [Token.Sub, IntLit 1, Token.Sub, Token.Add, IntLit 2]
                          , parseExpectedExpr = AST.Sub (AST.UMinus (Lit 1)) (AST.UPlus (Lit 2))
                          } 
             , ParseCase  { parseDescription = "ParseCase:  1 *  2"
                          , parseInput = "1 * 2"
                          , parseExpectedTokens = withPositions "1 * 2" [IntLit 1, Token.Mul, IntLit 2]
                          , parseExpectedExpr = AST.Mul (Lit 1) (Lit 2)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 /  2"
                          , parseInput = "1 / 2"
                          , parseExpectedTokens = withPositions "1 / 2" [IntLit 1, Token.Div, IntLit 2]
                          , parseExpectedExpr = AST.Div (Lit 1) (Lit 2)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * -2"
                          , parseInput = "1 * -2"
                          , parseExpectedTokens = withPositions "1 * -2" [IntLit 1, Token.Mul, Token.Sub, IntLit 2]
                          , parseExpectedExpr = AST.Mul (Lit 1) (AST.UMinus (Lit 2))
                          }        
             , ParseCase  { parseDescription = "ParseCase: -1 / +2"
                          , parseInput = "-1 / +2"
                          , parseExpectedTokens = withPositions "-1 / +2" [Token.Sub, IntLit 1, Token.Div, Token.Add, IntLit 2]
                          , parseExpectedExpr = AST.Div (AST.UMinus (Lit 1)) (AST.UPlus (Lit 2))
                          } 
             , ParseCase  { parseDescription = "ParseCase:  1 + 2 * 3"
                          , parseInput = "1 + 2 * 3"
                          , parseExpectedTokens = withPositions "1 + 2 * 3" [IntLit 1, Token.Add, IntLit 2, Token.Mul, IntLit 3]
                          , parseExpectedExpr = AST.Add (Lit 1) (AST.Mul (Lit 2) (Lit 3))
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * 2 + 3"
                          , parseInput = "1 * 2 + 3"
                          , parseExpectedTokens = withPositions "1 * 2 + 3" [IntLit 1, Token.Mul, IntLit 2, Token.Add, IntLit 3]
                          , parseExpectedExpr = AST.Add (AST.Mul (Lit 1) (Lit 2)) (Lit 3)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 + 2 + 3 + 4 + 5"
                          , parseInput = "1 + 2 + 3 + 4 + 5"
                          , parseExpectedTokens = withPositions "1 + 2 + 3 + 4 + 5" [IntLit 1, Token.Add, IntLit 2, Token.Add, IntLit 3, Token.Add, IntLit 4, Token.Add, IntLit 5]
                          , parseExpectedExpr = AST.Add (AST.Add (AST.Add (AST.Add (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 - 2 - 3 - 4 - 5"
                          , parseInput = "1 - 2 - 3 - 4 - 5"
                          , parseExpectedTokens = withPositions "1 - 2 - 3 - 4 - 5" [IntLit 1, Token.Sub, IntLit 2, Token.Sub, IntLit 3, Token.Sub, IntLit 4, Token.Sub, IntLit 5]
                          , parseExpectedExpr = AST.Sub (AST.Sub (AST.Sub (AST.Sub (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * 2 * 3 * 4 * 5"
                          , parseInput = "1 * 2 * 3 * 4 * 5"
                          , parseExpectedTokens = withPositions "1 * 2 * 3 * 4 * 5" [IntLit 1, Token.Mul, IntLit 2, Token.Mul, IntLit 3, Token.Mul, IntLit 4, Token.Mul, IntLit 5]
                          , parseExpectedExpr = AST.Mul (AST.Mul (AST.Mul (AST.Mul (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 / 2 / 3 / 4 / 5"
                          , parseInput = "1 / 2 / 3 / 4 / 5"
                          , parseExpectedTokens = withPositions "1 / 2 / 3 / 4 / 5" [IntLit 1, Token.Div, IntLit 2, Token.Div, IntLit 3, Token.Div, IntLit 4, Token.Div, IntLit 5]
                          , parseExpectedExpr = AST.Div (AST.Div (AST.Div (AST.Div (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 + 2 - 3 + 4 - 5"
                          , parseInput = "1 + 2 - 3 + 4 - 5"
                          , parseExpectedTokens = withPositions "1 + 2 - 3 + 4 - 5" [IntLit 1, Token.Add, IntLit 2, Token.Sub, IntLit 3, Token.Add, IntLit 4, Token.Sub, IntLit 5]
                          , parseExpectedExpr = AST.Sub (AST.Add (AST.Sub (AST.Add (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * 2 / 3 * 4 / 5"
                          , parseInput = "1 * 2 / 3 * 4 / 5"
                          , parseExpectedTokens = withPositions "1 * 2 / 3 * 4 / 5" [IntLit 1, Token.Mul, IntLit 2, Token.Div, IntLit 3, Token.Mul, IntLit 4, Token.Div, IntLit 5]
                          , parseExpectedExpr = AST.Div (AST.Mul (AST.Div (AST.Mul (Lit 1) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * 2 * 3 + 4 * 5"
                          , parseInput = "1 * 2 * 3 + 4 * 5"
                          , parseExpectedTokens = withPositions "1 * 2 * 3 + 4 * 5" [IntLit 1, Token.Mul, IntLit 2, Token.Mul, IntLit 3, Token.Add, IntLit 4, Token.Mul, IntLit 5]
                          , parseExpectedExpr = AST.Add (AST.Mul (AST.Mul (Lit 1) (Lit 2)) (Lit 3)) (AST.Mul (Lit 4) (Lit 5))
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 * 2 - 3 * 4 * 5"
                          , parseInput = "1 * 2 - 3 * 4 * 5"
                          , parseExpectedTokens = withPositions "1 * 2 - 3 * 4 * 5" [IntLit 1, Token.Mul, IntLit 2, Token.Sub, IntLit 3, Token.Mul, IntLit 4, Token.Mul, IntLit 5]
                          , parseExpectedExpr = AST.Sub (AST.Mul (Lit 1) (Lit 2)) (AST.Mul (AST.Mul (Lit 3) (Lit 4)) (Lit 5))
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 / 2 / 3 + 4 / 5"
                          , parseInput = "1 / 2 / 3 + 4 / 5"
                          , parseExpectedTokens = withPositions "1 / 2 / 3 + 4 / 5" [IntLit 1, Token.Div, IntLit 2, Token.Div, IntLit 3, Token.Add, IntLit 4, Token.Div, IntLit 5]
                          , parseExpectedExpr = AST.Add (AST.Div (AST.Div (Lit 1) (Lit 2)) (Lit 3)) (AST.Div (Lit 4) (Lit 5))
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 / 2 - 3 / 4 / 5"
                          , parseInput = "1 / 2 - 3 / 4 / 5"
                          , parseExpectedTokens = withPositions "1 / 2 - 3 / 4 / 5" [IntLit 1, Token.Div, IntLit 2, Token.Sub, IntLit 3, Token.Div, IntLit 4, Token.Div, IntLit 5]
                          , parseExpectedExpr = AST.Sub (AST.Div (Lit 1) (Lit 2)) (AST.Div (AST.Div (Lit 3) (Lit 4)) (Lit 5))
                          }
             , ParseCase  { parseDescription = "ParseCase:  1 + 2 * 3 - 4 / 5 + 6"
                          , parseInput = "1 + 2 * 3 - 4 / 5 + 6"
                          , parseExpectedTokens = withPositions "1 + 2 * 3 - 4 / 5 + 6" [IntLit 1, Token.Add, IntLit 2, Token.Mul, IntLit 3, Token.Sub, IntLit 4, Token.Div, IntLit 5, Token.Add, Token.IntLit 6]
                          , parseExpectedExpr = AST.Add (AST.Sub (AST.Add (Lit 1) (AST.Mul (Lit 2) (Lit 3))) (AST.Div (Lit 4) (Lit 5))) (Lit 6)
                          }
             , ParseCase  { parseDescription = "ParseCase: -1 + +2 * -3 - +4 / -5 + +6"
                          , parseInput = "-1 + +2 * -3 - +4 / -5 + +6"
                          , parseExpectedTokens = withPositions "-1 + +2 * -3 - +4 / -5 + +6" [Token.Sub, IntLit 1, Token.Add, Token.Add, IntLit 2, Token.Mul, Token.Sub, IntLit 3, Token.Sub, Token.Add, IntLit 4, Token.Div, Token.Sub, IntLit 5, Token.Add, Token.Add, Token.IntLit 6]
                          , parseExpectedExpr = AST.Add (AST.Sub (AST.Add (AST.UMinus (Lit 1)) (AST.Mul (AST.UPlus (Lit 2)) (AST.UMinus (Lit 3)))) (AST.Div (AST.UPlus (Lit 4)) (AST.UMinus (Lit 5)))) (AST.UPlus (Lit 6))
                          }
                  ]