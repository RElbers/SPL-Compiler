{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Shared.Result where

import           Control.Applicative
import           Control.Monad
-----------------
-- View result --
-----------------
data Result a = Ok a Context | Error String Context
    deriving (Show, Eq, Functor)

instance Applicative Result where
    pure a = Ok a (mkContext 0 0 0)
    (Ok f _)      <*> (Ok a ctx)    = Ok (f a) ctx
    (Ok f _)      <*> Error msg ctx = Error msg ctx
    Error msg ctx <*> _             = Error msg ctx

instance Monad Result where
    return = pure
    Ok a ctx >>= f = f a
    Error msg ctx >>= f = Error msg ctx
    fail msg = Error msg eof

instance Alternative Result where
    empty = Error "Unknown error." eof
    a@(Error _ _) <|> b@(Error _ _) = a
    a@(Error _ _) <|> b             = b
    a@(Ok _ _)    <|> _             = a

instance MonadPlus  Result where
    mzero = empty
    mplus = (<|>)

isOk :: Result a -> Bool
isOk (Ok _ _) = True
isOk _        = False

isError :: Result a -> Bool
isError x = not (isOk x)

containsError :: [Result a] -> Bool
containsError results = any isError results

-----------------
-- View result --
-----------------
viewResult :: Show a => Result a -> String -> String
viewResult (Ok res ctx) contents =  (show res) ++ "\n"
viewResult _ contents            = ""

viewErrors :: String -> [Result a] -> String
viewErrors str res = concat (fmap (viewError str) res)

viewError :: String -> Result a -> String
viewError input (Error str (ctx@Context {lineNum=lineNum}))
    = "\x1b[1m" ++ "Error in line " ++ show lineNum ++ ":" ++ "\n\x1b[32m" ++ errorMessage ++ "\x1b[0m\n" ++ contextMessage
    where
        mapLines f = \str -> concat (fmap f (lines str))
        input'  = map (\c -> if c == '\t' then ' ' else c) input
        errorMessage = mapLines (\l -> "\x1b[1m" ++ "  * " ++ l ++ "\n") str
        contextMessage = mapLines (\l -> "\x1b[34m  | \x1b[0m" ++ l ++ "\n")  (viewContext input' ctx)
viewError _ _ = ""

insertAt :: Int -> [a] -> [a] -> [a]
insertAt z y xs = as ++ y ++ bs
    where (as,bs) = splitAt z xs

viewContext :: String -> Context -> String
viewContext str (Context {lineNum=lineNum, startCol=startCol, endCol=endCol})
    = "\n" ++ line' ++ "\n" ++ indicators
        where line = lines str !! l
              line' = insertAt s "\x1b[91m" (insertAt e "\x1b[0m" line)
              indicators = "\x1b[91m" ++ take (s) (repeat ' ') ++ take (e - s) (repeat '^') ++ "\x1b[0m"
              s = startCol-1
              e = endCol
              l = lineNum-1

-------------
-- Context --
-------------
data Context = Context {lineNum :: Int, startCol :: Int, endCol :: Int}
    deriving (Show, Ord)

instance Eq Context where
    a == b = True

mkContext lineNum startCol endCol = Context {lineNum=lineNum, startCol=startCol, endCol=endCol}
eof = mkContext (-1) (-1) (-1)
zeroCtx = mkContext maxBound maxBound (-1)

(|+|) :: Context -> Context -> Context
(|+|) (Context {lineNum=lineNum, startCol=startCol, endCol=endCol})
      (Context {lineNum=lineNum', startCol=startCol', endCol=endCol'}) =
          Context {
                lineNum=(min lineNum lineNum'),
                startCol=(min startCol startCol'),
                endCol=(max endCol endCol')}

(|>|) :: Context -> Context -> Bool
(|>|) (Context {lineNum=lineNum, startCol=startCol, endCol=endCol})
      (Context {lineNum=lineNum', startCol=startCol', endCol=endCol'}) =
        lineNum > lineNum' || (lineNum == lineNum' && startCol > startCol')
