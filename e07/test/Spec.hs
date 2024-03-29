{-# LANGUAGE RecordWildCards #-}

import Data.Either (partitionEithers)
import Data.Foldable     (for_)
import Data.Function     (on)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List
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

stmtsFrom :: [Token] -> [Statement]
stmtsFrom = parse

specs :: Spec
specs = describe "Lexing and Parsing" $ for_ testCases test
    where test LexCase{..}   = it lexDescription $ (tokensFrom lexInput) `shouldBe` expectedTokens
          test ParseCase{..} = it parseDescription $ let tokens = tokensFrom parseInput 
                                                         in (stmtsFrom tokens) `shouldBe` expectedStmts

allTokens = ["identifier"
            , "12345"
            , "=="
            , "!="
            , "<="
            , ">="
            , "&&"
            , "||"
            , "if"
            , "else"
            , "while"
            , "print"
            , "int"
            , "boolean"
            , "true"
            , "false"
            , "("
            , ")"
            , "{"
            , "}"
            , ";"
            , ","
            , "="
            , "!"
            , "<"
            , ">"
            , "&"
            , "|"
            , "^"
            , "~"
            , "+"
            , "-"
            , "*"
            , "/"
            ]

allTokensOnDifferentLines = unlines $ allTokens
allTokensOnSameLine = intercalate " " allTokens

data TestCase = LexCase   { lexDescription :: String 
                      , lexInput :: String 
                      , expectedTokens :: [Token]
                      }
          | ParseCase { parseDescription :: String
                      , parseInput :: String
                      , expectedStmts :: [Statement]
                      }

testCases = lexCases ++ parseCases

lexCases :: [TestCase]
lexCases = [ LexCase { lexDescription = "LexCase: Longest match first"
                     , lexInput = "===!=!!====<=<>=>&&&|||"
                     , expectedTokens = [ Token.Eq          (position 1  1)
                                        , Token.Assignment  (position 1  3)
                                        , Token.NotEq       (position 1  4)
                                        , Token.Not         (position 1  6)
                                        , Token.NotEq       (position 1  7)
                                        , Token.Eq          (position 1  9)
                                        , Token.Assignment  (position 1 11)
                                        , Token.LessOrEq    (position 1 12)
                                        , Token.LessThan    (position 1 14)
                                        , Token.GreaterOrEq (position 1 15)
                                        , Token.GreaterThan (position 1 17)
                                        , Token.LogicalAnd  (position 1 18)
                                        , Token.BitAnd      (position 1 20)
                                        , Token.LogicalOr   (position 1 21)
                                        , Token.BitOr       (position 1 23)
                                        , Token.EndInput    start
                                        ]
                     }
           , LexCase { lexDescription = "LexCase: Keywords inside of identifiers"
                     , lexInput = "sift integrity elsewhere truer falseify printer awhile booleaned"
                     , expectedTokens = [ Token.Identifier "sift"      (position 1  1)
                                        , Token.Identifier "integrity" (position 1  6)
                                        , Token.Identifier "elsewhere" (position 1  16)
                                        , Token.Identifier "truer"     (position 1  26)
                                        , Token.Identifier "falseify"  (position 1  32)
                                        , Token.Identifier "printer"   (position 1  41)
                                        , Token.Identifier "awhile"    (position 1  49)
                                        , Token.Identifier "booleaned" (position 1  56)
                                        , Token.EndInput               start
                                        ]
                     }
           , LexCase { lexDescription = "LexCase: All possible tokens on same line"
                     , lexInput = allTokensOnSameLine
                     , expectedTokens = [ Token.Identifier "identifier" (position 1   1) 
                                        , Token.IntLit 12345            (position 1  12)
                                        , Token.Eq                      (position 1  18)
                                        , Token.NotEq                   (position 1  21)
                                        , Token.LessOrEq                (position 1  24)
                                        , Token.GreaterOrEq             (position 1  27)
                                        , Token.LogicalAnd              (position 1  30)
                                        , Token.LogicalOr               (position 1  33)
                                        , Token.If                      (position 1  36)
                                        , Token.Else                    (position 1  39)
                                        , Token.While                   (position 1  44)
                                        , Token.Print                   (position 1  50)
                                        , Token.Int'                    (position 1  56)
                                        , Token.Boolean                 (position 1  60)
                                        , Token.True'                   (position 1  68)
                                        , Token.False'                  (position 1  73)
                                        , Token.OpenParen               (position 1  79)
                                        , Token.CloseParen              (position 1  81)
                                        , Token.OpenBrace               (position 1  83)
                                        , Token.CloseBrace              (position 1  85)
                                        , Token.Semicolon               (position 1  87)
                                        , Token.Comma                   (position 1  89)
                                        , Token.Assignment              (position 1  91)
                                        , Token.Not                     (position 1  93)
                                        , Token.LessThan                (position 1  95)
                                        , Token.GreaterThan             (position 1  97)
                                        , Token.BitAnd                  (position 1  99)
                                        , Token.BitOr                   (position 1 101)
                                        , Token.BitXor                  (position 1 103)
                                        , Token.Tilde                   (position 1 105)
                                        , Token.Add                     (position 1 107)
                                        , Token.Sub                     (position 1 109)
                                        , Token.Mul                     (position 1 111)
                                        , Token.Div                     (position 1 113)
                                        , Token.EndInput                start
                                        ]
                     }
           , LexCase { lexDescription = "LexCase: All possible tokens on different lines"
                     , lexInput = allTokensOnDifferentLines
                     , expectedTokens = [ Token.Identifier "identifier" (position 1  1)
                                        , Token.IntLit 12345            (position 2  1)
                                        , Token.Eq                      (position 3  1)
                                        , Token.NotEq                   (position 4  1)
                                        , Token.LessOrEq                (position 5  1)
                                        , Token.GreaterOrEq             (position 6  1)
                                        , Token.LogicalAnd              (position 7  1)
                                        , Token.LogicalOr               (position 8  1)
                                        , Token.If                      (position 9  1)
                                        , Token.Else                    (position 10 1)
                                        , Token.While                   (position 11 1)
                                        , Token.Print                   (position 12 1)
                                        , Token.Int'                    (position 13 1)
                                        , Token.Boolean                 (position 14 1)
                                        , Token.True'                   (position 15 1)
                                        , Token.False'                  (position 16 1)
                                        , Token.OpenParen               (position 17 1)
                                        , Token.CloseParen              (position 18 1)
                                        , Token.OpenBrace               (position 19 1)
                                        , Token.CloseBrace              (position 20 1)
                                        , Token.Semicolon               (position 21 1)
                                        , Token.Comma                   (position 22 1)
                                        , Token.Assignment              (position 23 1)
                                        , Token.Not                     (position 24 1)
                                        , Token.LessThan                (position 25 1)
                                        , Token.GreaterThan             (position 26 1)
                                        , Token.BitAnd                  (position 27 1)
                                        , Token.BitOr                   (position 28 1)
                                        , Token.BitXor                  (position 29 1)
                                        , Token.Tilde                   (position 30 1)
                                        , Token.Add                     (position 31 1)
                                        , Token.Sub                     (position 32 1)
                                        , Token.Mul                     (position 33 1)
                                        , Token.Div                     (position 34 1)
                                        , Token.EndInput                start
                                        ]
                     }

           ]

parseCases :: [TestCase]
parseCases = [ 
               ParseCase  { parseDescription = "ParseCase: ;;;;"
                          , parseInput = ";;;;"
                          , expectedStmts = [Empty, Empty, Empty, Empty]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a || b;"
                          , parseInput = "i = a || b;"
                          , expectedStmts = [Assign "i" (AST.LOr (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a && b;"
                          , parseInput = "i = a && b;"
                          , expectedStmts = [Assign "i" (AST.LAnd (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a | b;"
                          , parseInput = "i = a | b;"
                          , expectedStmts = [Assign "i" (AST.BOr (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a ^ b;"
                          , parseInput = "i = a ^ b;"
                          , expectedStmts = [Assign "i" (AST.BXor (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a & b;"
                          , parseInput = "i = a & b;"
                          , expectedStmts = [Assign "i" (AST.BAnd (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a == b;"
                          , parseInput = "i = a == b;"
                          , expectedStmts = [Assign "i" (AST.Eql (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a != b;"
                          , parseInput = "i = a != b;"
                          , expectedStmts = [Assign "i" (AST.Neq (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a < b;"
                          , parseInput = "i = a < b;"
                          , expectedStmts = [Assign "i" (AST.Lt (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a > b;"
                          , parseInput = "i = a > b;"
                          , expectedStmts = [Assign "i" (AST.Gt (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a <= b;"
                          , parseInput = "i = a <= b;"
                          , expectedStmts = [Assign "i" (AST.Lte (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = a >= b;"
                          , parseInput = "i = a >= b;"
                          , expectedStmts = [Assign "i" (AST.Gte (AST.Identifier "a") (AST.Identifier "b"))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 + 2;"
                          , parseInput = "i = 1 + 2;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.IntLit 1) (AST.IntLit 2))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 - 2;"
                          , parseInput = "i = 1 - 2;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.IntLit 1) (AST.IntLit 2))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 + -2;"
                          , parseInput = "i = 1 + -2;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.IntLit 1) (AST.UMinus (AST.IntLit 2)))]
                          }        
             , ParseCase  { parseDescription = "ParseCase: i = -1 - +2;"
                          , parseInput = "i = -1 - +2;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.UMinus (AST.IntLit 1)) (AST.UPlus (AST.IntLit 2)))]
                          } 
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2;"
                          , parseInput = "i = 1 * 2;"
                          , expectedStmts = [Assign "i" (AST.Mul (AST.IntLit 1) (AST.IntLit 2))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 / 2;"
                          , parseInput = "i = 1 / 2;"
                          , expectedStmts = [Assign "i" (AST.Div (AST.IntLit 1) (AST.IntLit 2))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * -2;"
                          , parseInput = "i = 1 * -2;"
                          , expectedStmts = [Assign "i" (AST.Mul (AST.IntLit 1) (AST.UMinus (AST.IntLit 2)))]
                          }        
             , ParseCase  { parseDescription = "ParseCase: i = -1 / +2;"
                          , parseInput = "i = -1 / +2;"
                          , expectedStmts = [Assign "i" (AST.Div (AST.UMinus (AST.IntLit 1)) (AST.UPlus (AST.IntLit 2)))]
                          } 
             , ParseCase  { parseDescription = "ParseCase: i = 1 + 2 * 3;"
                          , parseInput = "i = 1 + 2 * 3;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.IntLit 1) (AST.Mul (AST.IntLit 2) (AST.IntLit 3)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2 + 3;"
                          , parseInput = "i = 1 * 2 + 3;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Mul (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = ((1) -  (2));"
                          , parseInput = "i = ((1) - (2));"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.IntLit 1) (AST.IntLit 2))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 + 2 + 3 + 4 + 5;"
                          , parseInput = "i = 1 + 2 + 3 + 4 + 5;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Add (AST.Add (AST.Add (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 - 2 - 3 - 4 - 5;"
                          , parseInput = "i = 1 - 2 - 3 - 4 - 5;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.Sub (AST.Sub (AST.Sub (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2 * 3 * 4 * 5;"
                          , parseInput = "i = 1 * 2 * 3 * 4 * 5;"
                          , expectedStmts = [Assign "i" (AST.Mul (AST.Mul (AST.Mul (AST.Mul (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 / 2 / 3 / 4 / 5;"
                          , parseInput = "i = 1 / 2 / 3 / 4 / 5;"
                          , expectedStmts = [Assign "i" (AST.Div (AST.Div (AST.Div (AST.Div (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 + 2 - 3 + 4 - 5;"
                          , parseInput = "i = 1 + 2 - 3 + 4 - 5;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.Add (AST.Sub (AST.Add (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2 / 3 * 4 / 5;"
                          , parseInput = "i = 1 * 2 / 3 * 4 / 5;"
                          , expectedStmts = [Assign "i" (AST.Div (AST.Mul (AST.Div (AST.Mul (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.IntLit 4)) (AST.IntLit 5))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2 * 3 + 4 * 5;"
                          , parseInput = "i = 1 * 2 * 3 + 4 * 5;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Mul (AST.Mul (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.Mul (AST.IntLit 4) (AST.IntLit 5)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 * 2 - 3 * 4 * 5;"
                          , parseInput = "i = 1 * 2 - 3 * 4 * 5;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.Mul (AST.IntLit 1) (AST.IntLit 2)) (AST.Mul (AST.Mul (AST.IntLit 3) (AST.IntLit 4)) (AST.IntLit 5)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 / 2 / 3 + 4 / 5;"
                          , parseInput = "i = 1 / 2 / 3 + 4 / 5;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Div (AST.Div (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3)) (AST.Div (AST.IntLit 4) (AST.IntLit 5)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 / 2 - 3 / 4 / 5;"
                          , parseInput = "i = 1 / 2 - 3 / 4 / 5;"
                          , expectedStmts = [Assign "i" (AST.Sub (AST.Div (AST.IntLit 1) (AST.IntLit 2)) (AST.Div (AST.Div (AST.IntLit 3) (AST.IntLit 4)) (AST.IntLit 5)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = 1 + 2 * 3 - 4 / 5 + 6;"
                          , parseInput = "i = 1 + 2 * 3 - 4 / 5 + 6;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Sub (AST.Add (AST.IntLit 1) (AST.Mul (AST.IntLit 2) (AST.IntLit 3))) (AST.Div (AST.IntLit 4) (AST.IntLit 5))) (AST.IntLit 6))]
                          }
             , ParseCase  { parseDescription = "ParseCase: i = -1 + +2 * -3 - +4 / -5 + +6;"
                          , parseInput = "i = -1 + +2 * -3 - +4 / -5 + +6;"
                          , expectedStmts = [Assign "i" (AST.Add (AST.Sub (AST.Add (AST.UMinus (AST.IntLit 1)) (AST.Mul (AST.UPlus (AST.IntLit 2)) (AST.UMinus (AST.IntLit 3)))) (AST.Div (AST.UPlus (AST.IntLit 4)) (AST.UMinus (AST.IntLit 5)))) (AST.UPlus (AST.IntLit 6)))]
                          }
             , ParseCase  { parseDescription = "ParseCase: {{ {i = 2;} {i = 5;} }}"
                          , parseInput = "{{ {i = 2;} {i = 5;} }}"
                          , expectedStmts = [AST.Block [AST.Block [AST.Block [Assign "i" (AST.IntLit 2)], AST.Block [Assign "i" (AST.IntLit 5)]]]]
                          }
             , ParseCase  { parseDescription = "ParseCase: while (i < 5) { i = i + 5; }"
                          , parseInput = "while (i < 5) { i = i + 5; }"
                          , expectedStmts = [AST.While (AST.Lt (AST.Identifier "i") (AST.IntLit 5)) (AST.Block [AST.Assign "i" (AST.Add (AST.Identifier "i") (AST.IntLit 5))])]
                          }
             , ParseCase  { parseDescription = "ParseCase: if (a > b) { m = 1 + 2; }"
                          , parseInput = "if (a > b) { m = 1 + 2; }"
                          , expectedStmts = [AST.If (AST.Gt (AST.Identifier "a") (AST.Identifier "b")) (AST.Block [AST.Assign "m" (AST.Add (AST.IntLit 1) (AST.IntLit 2))]) AST.Empty]
                          }
             , ParseCase  { parseDescription = "ParseCase: if (a < b) { m = 1 + 2; } else { n = 2 + 3; }"
                          , parseInput = "if (a > b) { m = 1 + 2; } else { n = 2 + 3; }" 
                          , expectedStmts = [AST.If (AST.Gt (AST.Identifier "a") (AST.Identifier "b")) (AST.Block [AST.Assign "m" (AST.Add (AST.IntLit 1) (AST.IntLit 2))]) (AST.Block [AST.Assign "n" (AST.Add (AST.IntLit 2) (AST.IntLit 3))])]
                          }
             , ParseCase  { parseDescription = "ParseCase: print (1 + 2 + 3);"
                          , parseInput = "print (1 + 2 + 3);" 
                          , expectedStmts = [AST.Print (AST.Add (AST.Add (AST.IntLit 1) (AST.IntLit 2)) (AST.IntLit 3))]
                          }
             , ParseCase  { parseDescription = "ParseCase: int i, j, k;"
                          , parseInput = "int i, j, k;"
                          , expectedStmts = [VarDecl AST.Int' [AST.Identifier "i", AST.Identifier "j", AST.Identifier "k"]]
                          }
             , ParseCase  { parseDescription = "ParseCase: boolean i, j, k;"
                          , parseInput = "boolean i, j, k;"
                          , expectedStmts = [VarDecl AST.Boolean [AST.Identifier "i", AST.Identifier "j", AST.Identifier "k"]]
                          }
                    
             ]