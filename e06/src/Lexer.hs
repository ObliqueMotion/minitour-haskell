module Lexer
    ( lexer
    ) where

import Token
import Data.Char
import Data.Map.Strict as DMS
import Diagnostic

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
lexer [] = [EndInput]
lexer input = let (token, rest) = nextToken input
              in token : (lexer rest)


nextToken :: String -> (Token, String)
nextToken ('=':'=':cs) = (Eq, cs)
nextToken ('!':'=':cs) = (NotEq, cs)
nextToken ('<':'=':cs) = (LessOrEq, cs)
nextToken ('>':'=':cs) = (GreaterOrEq, cs)
nextToken ('&':'&':cs) = (LogicalAnd, cs)
nextToken ('|':'|':cs) = (LogicalOr, cs)
nextToken ('/':'/':cs) = nextToken $ skipOneLineComment cs
nextToken ('(':cs)     = (OpenParen, cs)
nextToken (')':cs)     = (CloseParen, cs)
nextToken ('{':cs)     = (OpenBrace, cs) 
nextToken ('}':cs)     = (CloseBrace, cs) 
nextToken (';':cs)     = (Semicolon, cs)
nextToken (',':cs)     = (Comma, cs)
nextToken ('=':cs)     = (Assignment, cs)
nextToken ('!':cs)     = (Not, cs)
nextToken ('<':cs)     = (LessThan, cs)
nextToken ('>':cs)     = (GreaterThan, cs)
nextToken ('&':cs)     = (BitAnd, cs)
nextToken ('|':cs)     = (BitOr, cs)
nextToken ('^':cs)     = (BitXor, cs)
nextToken ('~':cs)     = (Tilde, cs)
nextToken ('+':cs)     = (Add, cs)
nextToken ('-':cs)     = (Sub, cs)
nextToken ('*':cs)     = (Mul, cs)
nextToken ('/':cs)     = (Div, cs)
nextToken (c:cs)
        | isSpace c = nextToken cs
        | isDigit c = let (num, rest) = number ([c], cs)
                      in (IntLit (read num), rest)
        | isJavaIdentifierStart c = let (id, rest) = identifier ([c], cs)
                                    in case DMS.lookup id reserved of
                                        Just token -> (token, rest)    
                                        Nothing    -> (Identifier id, rest)
        | otherwise = (Error $ invalidCharacter c (position 0 0), cs)
nextToken _ = (EndInput, "")

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