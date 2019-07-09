{-# LANGUAGE TypeSynonymInstances #-}

module Parsing.Lexer where

import           Prelude       hiding (Word)

import           Control.Monad
import           Data.Char
import           Shared.AST
import           Shared.Result

-----------
-- Lexer --
-----------
data Token = ReservedToken String -- ReservedTokens are keywords, operators and symbols.
    | BoolToken Bool
    | NumToken Int
    | StringToken String
    deriving (Eq)

instance Show Token where
    show (ReservedToken s) = s
    show (BoolToken b)     =  show b
    show (NumToken n)      =  show n
    show (StringToken s)   =  s

-- Tokenizer
type Character = (Char, Int, Int) -- Char, line-number, col-number
type Word = (String, Context) -- String, line-number, start-col, end-col

(===) :: String -> [Character] -> Bool
(===) [] []                   = True
(===) (c:s) []                = False
(===) [] (c:s)                = False
(===) (c1:s1) ((c2, _, _):s2) = c1 == c2 && s1 === s2


lexer :: String -> [Result Token]
lexer = tokenize . removeComments . addContextToChars

-- Adds line and column numbers to every character
addContextToChars :: String -> [Character]
addContextToChars str = addContextToChars' str 1 1
    where
        addContextToChars' ('\n':str) line col = (('\n', line, col):addContextToChars' str (line+1) 1)
        addContextToChars' (c:str)    line col = ((c, line, col):addContextToChars' str line (col+1))
        addContextToChars' []         _    _   = []

-- Removes comments and block comments from input.
removeComments :: [Character] -> [Character]
removeComments [] = []
removeComments str@(hd:tl)
    | startsWith "//" str = removeComments (skipTill "\n" str)
    | startsWith "/*" str = removeComments (skipTill "*/" str)
    | otherwise          = hd:removeComments tl

startsWith :: String -> [Character] -> Bool
startsWith [] s                  = True
startsWith s []                  = False
startsWith (c:s) ((c', _, _):s') = c == c' && startsWith s s'

skipTill :: String -> [Character] -> [Character]
skipTill [] s = s
skipTill s [] = []
skipTill (c:s) ((c', _, _):s')
    | c == c' = skipTill s s'
    | otherwise = skipTill (c:s) s'

-- Converts the input into a list of words, with the starting column and ending column set.
tokenize :: [Character] -> [Result Token]
tokenize [] = []
tokenize chars
    | Just (str, ctx, rest) <- split isWhitespaceChar chars =  tokenize rest
    | Just (str, ctx, rest) <- split isIdentifier chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "->" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "::" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "<=" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix ">=" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "!=" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "&&" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "||" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "==" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "+" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "-" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "*" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "/" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "%" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "<" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix ">" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix ":" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "!" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "=" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix ";" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "(" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix ")" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "[" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "]" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "{" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "}" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "," chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "." chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | Just (str, ctx, rest) <- stripPrefix "'" chars = (Ok (stringToToken str) (ctx)) :  tokenize rest
    | otherwise = case split (\c -> not (isWhitespaceChar c)) chars of
        Just (str, ctx, rest) -> (Error ("Unrecognized token: '" ++ str ++ "'") ctx) : tokenize rest
        Nothing -> []

isIdentifier = \c -> isAlphaNum c || c == '_'
isWhitespaceChar = \c -> elem c [' ', '\t', '\r', '\n']

-- Split a list of Characters by a predicate.
split :: (Char -> Bool) -> [Character] -> Maybe (String, Context, [Character])
split pred chars = case split' pred chars of
    Nothing              -> Nothing
    Just (pattern, rest) -> Just (stringify pattern, context pattern, rest )
    where
        split' pred [] = Nothing
        split' pred (char@(c',_,_):s')
            | pred c' = add char s' (split' pred s')
            | otherwise = Nothing

add :: Character -> [Character] -> Maybe ([Character], [Character]) -> Maybe ([Character], [Character])
add char rest (Just (xs, yz)) = Just ((char:xs), yz)
add char rest Nothing         = Just ([char], rest)

-- Split a list of Characters by a string pattern.
stripPrefix :: String -> [Character] -> Maybe (String, Context, [Character])
stripPrefix str [] = Nothing
stripPrefix str chars = case stripPrefix' str chars of
    Nothing -> Nothing
    Just (pattern, rest) | str === pattern -> Just (stringify pattern, context pattern, rest )
                         | otherwise       -> Nothing
    where
        stripPrefix' pattern [] = Nothing
        stripPrefix' [] chars = Just ([], chars)
        stripPrefix' (c:s) (char@(c',_,_):s')
            | c == c' = add char s' (stripPrefix' s s')
            | otherwise = Nothing

toWord :: [Character] -> Word
toWord []    = ([], mkContext 0 0 0)
toWord chars = (string, mkContext line startCol endCol)
    where
        string = fmap (\(s,_,_) -> s) chars
        line = minimum (fmap (\(_,l,_) -> l) chars)
        startCol = minimum (fmap (\(_,_,c) -> c) chars)
        endCol = maximum (fmap (\(_,_,c) -> c) chars)

context :: [Character] -> Context
context []    = mkContext 0 0 0
context chars = mkContext line startCol endCol
    where
        line = minimum (fmap (\(_,l,_) -> l) chars)
        startCol = minimum (fmap (\(_,_,c) -> c) chars)
        endCol = maximum (fmap (\(_,_,c) -> c) chars)

stringify :: [Character] -> String
stringify chars = fmap (\(s,_,_) -> s) chars

strToBool "True"  = True
strToBool "False" = False

isInt :: String -> Bool
isInt str = all isDigit str
isId :: String -> Bool
isId str = all isIdentifier str

stringToToken :: String -> Token
stringToToken str
    | elem str (keywords ++ operators ++ otherSymbols) = (ReservedToken str)
    | elem str booleans = (BoolToken (strToBool str))
    | isInt str = (NumToken (read str))
    | isId str = (StringToken str)


keywords = [
    "var",
    "Void",
    "if",
    "then",
    "else",
    "while",
    "return",
    "Int",
    "Bool",
    "Char",
    "class"]

operators = [
    "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    "<=",
    ">=",
    "<",
    ">",
    "!=",
    "&&",
    "||",
    ":",
    "!",
    "."]

otherSymbols = [
    "->",
    "::",
    "=",
    ";",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    ",",
    ".",
    "'"]

booleans = [
    "True",
    "False"]
