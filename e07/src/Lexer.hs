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


lexer :: String -> ([Either Token Diagnostic])
lexer [] = []
lexer input = let (maybeToken, pos, css) = nextToken (position 1 1) input
              in maybeToken : lexer' pos css
              where lexer' _ [] = []
                    lexer' pos' css' = let (token', pos'', css'') = nextToken pos' css'
                                        in token' : (lexer' pos'' css'')


nextToken :: Position -> String -> (Either Token Diagnostic, Position, String)
nextToken pos ('=':'=':cs) = (Left Eq, incCol 2 pos, cs)
nextToken pos ('!':'=':cs) = (Left NotEq, incCol 2 pos, cs)
nextToken pos ('<':'=':cs) = (Left LessOrEq, incCol 2 pos, cs)
nextToken pos ('>':'=':cs) = (Left GreaterOrEq, incCol 2 pos, cs)
nextToken pos ('&':'&':cs) = (Left LogicalAnd, incCol 2 pos, cs)
nextToken pos ('|':'|':cs) = (Left LogicalOr, incCol 2 pos, cs)
nextToken pos ('/':'/':cs) = nextToken (incRow 1 pos) (skipOneLineComment cs)
nextToken pos ('(':cs)     = (Left OpenParen, incCol 1 pos, cs)
nextToken pos (')':cs)     = (Left CloseParen, incCol 1 pos, cs)
nextToken pos ('{':cs)     = (Left OpenBrace, incCol 1 pos, cs) 
nextToken pos ('}':cs)     = (Left CloseBrace, incCol 1 pos, cs) 
nextToken pos (';':cs)     = (Left Semicolon, incCol 1 pos, cs)
nextToken pos (',':cs)     = (Left Comma, incCol 1 pos, cs)
nextToken pos ('=':cs)     = (Left Assignment, incCol 1 pos, cs)
nextToken pos ('!':cs)     = (Left Not, incCol 1 pos, cs)
nextToken pos ('<':cs)     = (Left LessThan, incCol 1 pos, cs)
nextToken pos ('>':cs)     = (Left GreaterThan, incCol 1 pos, cs)
nextToken pos ('&':cs)     = (Left BitAnd, incCol 1 pos, cs)
nextToken pos ('|':cs)     = (Left BitOr, incCol 1 pos, cs)
nextToken pos ('^':cs)     = (Left BitXor, incCol 1 pos, cs)
nextToken pos ('~':cs)     = (Left Tilde, incCol 1 pos, cs)
nextToken pos ('+':cs)     = (Left Add, incCol 1 pos, cs)
nextToken pos ('-':cs)     = (Left Sub, incCol 1 pos, cs)
nextToken pos ('*':cs)     = (Left Mul, incCol 1 pos, cs)
nextToken pos ('/':cs)     = (Left Div, incCol 1 pos, cs)
nextToken pos (c:cs)
        | '\n' == c = nextToken (incRow 1 pos) cs
        | isSpace c = nextToken (incCol 1 pos) cs
        | isDigit c = let (numTail, css) = number cs
                          num = c:numTail
                      in (Left $ IntLit (read (num)), incCol (length (num)) pos, css)
        | isJavaIdentifierStart c = let (idTail, css) = identifier cs
                                        id = c:idTail
                                    in case DMS.lookup id reserved of
                                        Just token -> (Left token, incCol (length id) pos, css)    
                                        Nothing    -> (Left $ Identifier id, incCol (length id) pos, css)
        | otherwise = (Right $ invalidCharacter c pos, pos, cs)
nextToken pos _ = (Left EndInput, pos, "")

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

number :: String -> (String, String)
number = span isDigit

identifier :: String -> (String, String)
identifier = span isJavaIdentifierPart