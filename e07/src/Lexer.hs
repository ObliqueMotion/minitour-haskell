module Lexer
    ( Lexer.lex
    ) where

import Token
import Data.Char
import Data.Map.Strict as DMS
import Control.Applicative
import Diagnostic
import Position
import Data.Either

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> (String -> [(a, String)])
parse (P p) inp = p inp

instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap f parser = do x <- parser
                           return (f x)

instance Applicative Parser where
        -- pure :: a -> Parser a
        pure x = P (\inp -> [(x, inp)])

        -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pf <*> px = do f <- pf
                       x <- px
                       return (f x)

instance Monad Parser where
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        px >>= f = P (\inp -> case parse px inp of
                                   []         -> []
                                   [(x, out)] -> parse (f x) out)


instance Alternative Parser where
     -- empty :: Parser a
     empty = P (\_ -> [])

     -- (<|>) :: Parser a -> Parser a -> Parser a
     px <|> py = P (\inp -> case parse px inp of 
                                 []  -> parse py inp
                                 [(y, out)] -> [(y, out)])
                                    

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])
                                   
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else Control.Applicative.empty

char :: Char -> Parser Char
char = sat . (==)
                                   
digit :: Parser Char
digit = sat isDigit

int :: Parser Int
int = do xs <- some digit
         return (read xs)
                                   
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
                                   
skipOne :: Parser ()
skipOne = do item
             return ()

skipSpace :: Parser ()
skipSpace = do many (sat isSpace)
               return ()

skipUntilNext :: String -> Parser ()
skipUntilNext s = do string s
                     return ()
              <|> do skipOne
                     skipUntilNext s

intLit :: Parser Token
intLit = do x <- int
            return (IntLit x)

ident :: Parser Token
ident = do x  <- sat isJavaIdentifierStart
           xs <- many (sat isJavaIdentifierPart)
           return (case DMS.lookup (x:xs) reserved of
                        Just token -> token
                        Nothing    -> (Identifier (x:xs)))

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
                         
lex :: String -> ([Token], [Diagnostic])
lex = partitionEithers . fst . head . parse lexer 

lexer :: Parser [Either Token Diagnostic]
lexer = many (do skipSpace 
                 nextToken)


skipComments :: Parser ()
skipComments = do string "//"
                  skipUntilNext "\n"
              <|> do string "/*"
                     skipUntilNext "*/"

symbol :: String -> Token -> Parser Token
symbol s t = do string s
                return t

nextToken :: Parser (Either Token Diagnostic)
nextToken = do  skipComments
                nextToken
            <|> Left <$> ident
            <|> Left <$> intLit
            <|> Left <$> symbol "==" Eq
            <|> Left <$> symbol "!=" NotEq
            <|> Left <$> symbol "<=" LessOrEq
            <|> Left <$> symbol ">=" GreaterOrEq
            <|> Left <$> symbol "&&" LogicalAnd
            <|> Left <$> symbol "||" LogicalOr
            <|> Left <$> symbol "("  OpenParen
            <|> Left <$> symbol ")"  CloseParen
            <|> Left <$> symbol "{"  OpenBrace
            <|> Left <$> symbol "}"  CloseBrace
            <|> Left <$> symbol ";"  Semicolon
            <|> Left <$> symbol ","  Comma
            <|> Left <$> symbol "="  Assignment
            <|> Left <$> symbol "!"  Not
            <|> Left <$> symbol "<"  LessThan
            <|> Left <$> symbol ">"  GreaterThan
            <|> Left <$> symbol "&"  BitAnd
            <|> Left <$> symbol "|"  BitOr
            <|> Left <$> symbol "^"  BitXor
            <|> Left <$> symbol "~"  Tilde
            <|> Left <$> symbol "+"  Add
            <|> Left <$> symbol "-"  Sub
            <|> Left <$> symbol "*"  Mul
            <|> Left <$> symbol "/"  Div
        
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