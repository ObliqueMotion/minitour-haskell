module Lexer
    ( Lexer.lex
    ) where

import Token
import Data.Char
import Data.Map.Strict as DMS
import Combinator
import Control.Applicative
import Diagnostic
import Position
import Data.Either

reserved = DMS.fromList [ ("==",      Eq)
                        , ("!=",      NotEq)
                        , ("<=",      LessOrEq)
                        , (">=",      GreaterOrEq)
                        , ("&&",      LogicalAnd)
                        , ("||",      LogicalOr)
                        , ("(" ,      OpenParen)
                        , (")" ,      CloseParen)
                        , ("{" ,      OpenBrace)
                        , ("}" ,      CloseBrace)
                        , (";" ,      Semicolon)
                        , ("," ,      Comma)
                        , ("=" ,      Assignment)
                        , ("!" ,      Not)
                        , ("<" ,      LessThan)
                        , (">" ,      GreaterThan)
                        , ("&" ,      BitAnd)
                        , ("|" ,      BitOr)
                        , ("^" ,      BitXor)
                        , ("~" ,      Tilde)
                        , ("+" ,      Add)
                        , ("-" ,      Sub)
                        , ("*" ,      Mul)
                        , ("/" ,      Div)
                        , ("if",      If)
                        , ("int",     Int')
                        , ("else",    Else)
                        , ("true",    True')
                        , ("false",   False')
                        , ("print",   Print)
                        , ("while",   While)
                        , ("boolean", Boolean)
                        ]

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

invalidCharacter :: Char -> Position -> Diagnostic
invalidCharacter c p = failure ("Invalid character (" ++ [c] ++ ")") p

type Lexer   = Combinator (String, Position)
type PInt    = (Int,    Position)
type PChar   = (Char,   Position)
type PString = (String, Position)

item :: Lexer PChar
item = C (\inp -> case inp of
                  ([], _)     -> []
                  ((x:xs), p) -> [((x, p),(xs, p +> x))])
                                   
satisfy :: (Char -> Bool) -> Lexer PChar
satisfy f = do (x, p) <- item
               if f x then return (x, p)
                      else Control.Applicative.empty

char :: Char -> Lexer PChar
char = satisfy . (==)
                                   
digit :: Lexer PChar
digit = satisfy isDigit

int :: Lexer PInt
int = do xps <- some digit
         let p  = snd $ head xps
         let xs = fst <$> xps
         return (read xs, p)
                                   
string :: String -> Lexer PString
string []     = return ([], start)
string (x:xs) = do (_,p) <- char x
                   string xs
                   return ((x:xs), p)
                                   
skipOne :: Lexer ()
skipOne = do item
             return ()

skipSpace :: Lexer ()
skipSpace = do many (satisfy isSpace)
               return ()

skipUntilNext :: String -> Lexer ()
skipUntilNext s = do string s
                     return ()
              <|> do skipOne
                     skipUntilNext s

intLit :: Lexer Token
intLit = do (x,p) <- int
            return (IntLit x p)

ident :: Lexer Token
ident = do (x,p) <- satisfy isJavaIdentifierStart
           xps   <- many (satisfy isJavaIdentifierPart)
           let xs = fst <$> xps
           return (case DMS.lookup (x:xs) reserved of
                        Just token -> token p
                        Nothing    -> (Identifier (x:xs) p))

skipComments :: Lexer ()
skipComments = do string "//"
                  skipUntilNext "\n"
              <|> do string "/*"
                     skipUntilNext "*/"

symbol :: String -> Lexer Token
symbol s = do (_,p) <- string s
              case DMS.lookup s reserved of
                              Just t  -> return (t p)
                              Nothing -> Control.Applicative.empty

token :: Lexer (Either Token Diagnostic)
token  = do skipComments
            skipSpace
            token
        <|> Left <$> ident
        <|> Left <$> intLit
        <|> Left <$> symbol "=="
        <|> Left <$> symbol "!="
        <|> Left <$> symbol "<="
        <|> Left <$> symbol ">="
        <|> Left <$> symbol "&&"
        <|> Left <$> symbol "||"
        <|> Left <$> symbol "(" 
        <|> Left <$> symbol ")" 
        <|> Left <$> symbol "{" 
        <|> Left <$> symbol "}" 
        <|> Left <$> symbol ";" 
        <|> Left <$> symbol "," 
        <|> Left <$> symbol "=" 
        <|> Left <$> symbol "!" 
        <|> Left <$> symbol "<" 
        <|> Left <$> symbol ">" 
        <|> Left <$> symbol "&" 
        <|> Left <$> symbol "|" 
        <|> Left <$> symbol "^" 
        <|> Left <$> symbol "~"
        <|> Left <$> symbol "+" 
        <|> Left <$> symbol "-" 
        <|> Left <$> symbol "*" 
        <|> Left <$> symbol "/" 
        <|> do (c,p) <- item
               Right <$> return (invalidCharacter c p)

tokenize :: Lexer [Either Token Diagnostic]
tokenize = many (do skipSpace 
                    token)

lex :: String -> ([Token], [Diagnostic])
lex input = partitionEithers $ fst $ head $ apply tokenize (input, start)