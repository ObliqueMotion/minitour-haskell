module Lexer
    ( lexer
    ) where

import Token
import Data.Char
import Data.Map.Strict as DMS
import Diagnostic
import Position

invalidCharacter :: Char -> Position -> Diagnostic
invalidCharacter c p = failure ("Invalid character (" ++ [c] ++ ")") p

reserved = DMS.fromList [ ("if",      If)
                        , ("int",     Int')
                        , ("else",    Else)
                        , ("true",    True')
                        , ("false",   False')
                        , ("print",   Print)
                        , ("while",   While)
                        , ("boolean", Boolean)
                        ]


lexer :: String -> [Token]
lexer [] = []
lexer input = let (token, pos, rest) = nextToken (position 1 1) input
              in token : lexer' pos rest
              where lexer' _ [] = []
                    lexer' pos' rest' = let (token', pos'', rest'') = nextToken pos' rest'
                                        in token' : (lexer' pos'' rest'')


nextToken :: Position -> String -> (Token, Position, String)
nextToken pos ('=':'=':cs) = (Eq, incCol 2 pos, cs)
nextToken pos ('!':'=':cs) = (NotEq, incCol 2 pos, cs)
nextToken pos ('<':'=':cs) = (LessOrEq, incCol 2 pos, cs)
nextToken pos ('>':'=':cs) = (GreaterOrEq, incCol 2 pos, cs)
nextToken pos ('&':'&':cs) = (LogicalAnd, incCol 2 pos, cs)
nextToken pos ('|':'|':cs) = (LogicalOr, incCol 2 pos, cs)
nextToken pos ('/':'/':cs) = nextToken (incRow 1 pos) (skipOneLineComment cs)
nextToken pos ('(':cs)     = (OpenParen, incCol 1 pos, cs)
nextToken pos (')':cs)     = (CloseParen, incCol 1 pos, cs)
nextToken pos ('{':cs)     = (OpenBrace, incCol 1 pos, cs) 
nextToken pos ('}':cs)     = (CloseBrace, incCol 1 pos, cs) 
nextToken pos (';':cs)     = (Semicolon, incCol 1 pos, cs)
nextToken pos (',':cs)     = (Comma, incCol 1 pos, cs)
nextToken pos ('=':cs)     = (Assignment, incCol 1 pos, cs)
nextToken pos ('!':cs)     = (Not, incCol 1 pos, cs)
nextToken pos ('<':cs)     = (LessThan, incCol 1 pos, cs)
nextToken pos ('>':cs)     = (GreaterThan, incCol 1 pos, cs)
nextToken pos ('&':cs)     = (BitAnd, incCol 1 pos, cs)
nextToken pos ('|':cs)     = (BitOr, incCol 1 pos, cs)
nextToken pos ('^':cs)     = (BitXor, incCol 1 pos, cs)
nextToken pos ('~':cs)     = (Tilde, incCol 1 pos, cs)
nextToken pos ('+':cs)     = (Add, incCol 1 pos, cs)
nextToken pos ('-':cs)     = (Sub, incCol 1 pos, cs)
nextToken pos ('*':cs)     = (Mul, incCol 1 pos, cs)
nextToken pos ('/':cs)     = (Div, incCol 1 pos, cs)
nextToken pos (c:cs)
        | '\n' == c = nextToken (incRow 1 pos) cs
        | isSpace c = nextToken (incCol 1 pos) cs
        | isDigit c = let (num, rest) = number ([c], cs)
                      in (IntLit (read num), incCol (length num) pos, rest)
        | isJavaIdentifierStart c = let (id, rest) = identifier ([c], cs)
                                    in case DMS.lookup id reserved of
                                        Just token -> (token, incCol (length id) pos, rest)    
                                        Nothing    -> (Identifier id, incCol (length id) pos, rest)
        | otherwise = (Error $ invalidCharacter c pos, pos, cs)
nextToken pos _ = (EndInput, pos, "")

skipOneLineComment :: String -> String
skipOneLineComment [] = []
skipOneLineComment ('\n':cs) = cs
skipOneLineComment (c:cs) = skipOneLineComment cs

isJavaIdentifierStart :: Char -> Bool
isJavaIdentifierStart c
        | isAlpha c = True
        | c == '_'  = True
        | c == '$'  = True
        | otherwise = False

isJavaIdentifierPart :: Char -> Bool
isJavaIdentifierPart c
        | isAlphaNum c = True
        | c == '$'     = True
        | otherwise    = False

number :: (String, String) -> (String, String)
number (num, (c:cs))
        | isDigit c = number (num ++ [c], cs)
        | otherwise = (num, c:cs)

identifier :: (String, String) -> (String, String)
identifier (id, (c:cs))
        | isJavaIdentifierPart c = identifier (id ++ [c], cs)
        | otherwise = (id, c:cs)