{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parsing.ParserBase where

import           Parsing.Lexer
import           Prelude       hiding (return, (*>), (<$), (<$>), (<*), (<*>),
                                (<:>), (<|>), (>>=))
import           Shared.Result


------------
-- Errors --
------------
unexpectedToken expected actual ctx = Error ("Unexpected token. Expected \"" ++ expected ++ "\" but found " ++ show actual ++".") ctx
unexpectedEOF expected  ctx = Error ("Unexpected EOF. Expected " ++ expected ++ ".") ctx
unparsedInput ctx = Error ("Unparsed input remaining.") ctx

------------------
-- Parser Monad --
------------------
type Parser s r =  [(s, Context)] -> (Result r, [(s, Context)])

infixr 4 <|>
infixl 6 <*>, <*, *>, <!
infixr 7 <$>, <$

-- Parser combinators
(<$>) :: (r -> t) -> (Parser s r) -> Parser s t
(<$>) f g = \tks -> case (g tks) of (res, s) -> (fmap f res, s)

(<$) :: r -> (Parser s t) -> Parser s r
(<$) f g = return f <* g

-- For passing arguments
(<*>) :: (Parser s (r->t)) -> (Parser s r) -> Parser s t
p <*> q =
    (\input -> case p input of
        (Ok r1 c1, input1) -> case q input1 of
            (Ok r2 c2, input2) -> (Ok (r1 r2) (c1 |+| c2), input2 )
            (Error m c, s)     ->  (Error m c, [])
        (Error m c, s) ->  (Error m c, []))

-- Ignore left parser
(*>) ::(Parser s r) -> (Parser s t) -> Parser s t
p1 *> p2 = (\_ y -> y) <$> p1 <*> p2

-- Ignore right parser
(<*) ::(Parser s r) -> (Parser s t) -> Parser s r
p1 <* p2 = (\x _ -> x) <$> p1 <*> p2

-- Options
(<|>) :: (Parser s r) -> (Parser s r) -> Parser s r
p1 <|> p2 =  (\input -> case p1 input of
        e1@(Error _ c1, _) -> case p2 input of
            e2@(Error _ c2, _) -> if c1 |>| c2 then e1 else e2
            (res, tks)         -> (res, tks)
        (res, tks) -> (res, tks))

-- Kleene star
(|*) :: (Parser s r) -> Parser s [r]
(|*) p = (|+) p <|> return []
(|+) :: (Parser s r) -> Parser s [r]
(|+) p = (:) <$> p <*> (|*)  p

-- Parser for maybe
(|?) :: (Parser s r) -> Parser s (Maybe r)
(|?) p = Just <$> p <|> return Nothing

-- Collect contexts; 'witness'
(<!) :: (Parser s (Context -> t)) -> (Parser s Context) -> Parser s t
p <! q =
    (\input -> case p input of
        (Ok r1 c1, input1) -> case q input1 of
            _ -> (Ok (r1 c1) c1, input1)
        (Error m c, s) -> (Error m c, s))

-- Return
return :: r -> Parser s r
return a []                    = (Ok a zeroCtx, [])
return a input@((_, c) : rest) = (Ok a c, input)

-- Check token against string.
reserved str  [] = (unexpectedEOF str eof,  [])
reserved str ((sym, c) : rest)
    | sym == (ReservedToken str) = (Ok sym c,  rest)
    | otherwise  = (unexpectedToken str (show sym) c,  [])

-- Wrap parser with parentheses.
wrap beginStr endStr p = reserved beginStr *> p <* reserved endStr
parens p   = wrap "(" ")" p
brackets p = wrap "[" "]" p
curly p    = wrap "{" "}" p
quotes p   = wrap "'" "'" p

choice :: [Parser t r] -> Parser t r
choice [x]    =  x
choice (x:xs) = x <|> choice xs

(<**>) :: Parser s r -> Parser s (r -> r') -> Parser s r'
p1 <**> p2 = (\a b -> b a) <$> p1 <*> p2


(<??>) :: Parser s r -> Parser s (r -> r) -> Parser s r
p1 <??> p2 = p1 <**> (p2 <|> return id)

applyAll :: a -> [a -> a] -> a
applyAll x []     = x
applyAll x (f:fs) = applyAll (f x) fs

--      :: (Exp -> Exp -> Exp) -> Exp -> Exp
pChainL :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainL op p = applyAll <$> p <*> (|*) (flip <$> op <*> p)

pChainR :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainR op p = r
    where r = p <??> (flip <$> op <*> pChainR op p)

endOfFile []                = (Ok "" eof,  [])
endOfFile ((sym, c) : rest) = (unparsedInput c,  [])

---------------------------
-- Base parsable classes --
---------------------------
class Parsable a where
    parse :: Parser Token a

instance Parsable String where
    parse []                          = (unexpectedEOF "String" eof,  [])
    parse ((StringToken s, c) : rest) = (Ok s c, rest)
    parse ((sym, c) : rest)           = (unexpectedToken "String" (show sym) c,  [])

instance Parsable Int where
    parse []                       = (unexpectedEOF "Number" eof,  [])
    parse ((NumToken n, c) : rest) = (Ok n c, rest)
    parse ((sym, c) : rest)        = (unexpectedToken "Number" (show sym) c,  [])

instance Parsable Char where
    parse []                                 = (unexpectedEOF "Character" eof,  [])
    parse ((StringToken (char:_), c) : rest) = (Ok char c, rest)
    parse ((sym, c) : rest)                  = (unexpectedToken "Character" (show sym) c,  [])

instance Parsable Bool where
    parse []                        = (unexpectedEOF "Bool" eof,  [])
    parse ((BoolToken b, c) : rest) = (Ok b c, rest)
    parse ((sym, c) : rest)         = (unexpectedToken "Bool" (show sym) c,  [])

instance Parsable Context where
    parse input = (Ok zeroCtx zeroCtx, input)
