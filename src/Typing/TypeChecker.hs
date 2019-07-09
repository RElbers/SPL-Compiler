{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typing.TypeChecker where

import           Control.Monad       hiding (return)
import           Control.Monad.State hiding (return)
import qualified Data.List
import           Data.Map            (Map, elems, fromList, insert, map)
import           Data.Map            hiding (lookup)
import           Parsing.Lexer
import           Parsing.ParserBase  hiding (return, (<$>), (<*>))
import           Parsing.ParserImpl
import           Prelude             hiding (delete, insert, lookup, return)
import           Shared.AST          hiding (symbols)
import           Shared.Result
import           Shared.SymbolTable

typeInference :: SPL -> SymbolTable -> Result SymbolTable
typeInference spl symbols =
    let
        symbols = (buildSymbolTable spl)
        newState = InferenceState {
            n=1,
            symbols=symbols,
            scope="*"
        }

        state = execStateT (infer Untyped spl ) newState
    in
        fmap (\InferenceState{..} -> symbols) state

evalInference :: SPL -> SymbolTable -> Result Substitutions
evalInference spl symbols =
    let
        symbols = (buildSymbolTable spl)
        newState = InferenceState {
            n=1,
            symbols=symbols,
            scope="*"
        }
        subs = evalStateT (infer Untyped spl ) newState
    in
        subs


------------
-- Errors --
------------
freeTypeVarError :: String -> String
freeTypeVarError id = "Type variable " ++ id ++ " is not free."
unificationError :: Type -> Type -> String
unificationError type1 type2 = ("Unable to unify expected type '" ++ show type1 ++ "' with actual type '" ++ show type2 ++ "'")
undeclaredFunction :: String -> String
undeclaredFunction id = ("Undeclared function '" ++ id ++ "'")
undeclaredVar :: String -> String
undeclaredVar id = ("Undeclared variable '" ++ id ++ "'")
argumentLengthError :: Int -> Int -> String
argumentLengthError a b = "Number of formal arguments is different from number of actual arguments. " ++ (show $ a) ++ " vs " ++ (show $ b)
argumentLengthError2 ::  String
argumentLengthError2 = "Number of arguments is different then number of argument types. "

-------------------
-- Substitutions --
-------------------
type Substitutions = [(Type, Type)]

infixr 3 <+>
(<+>) :: Substitutions -> Substitutions -> Substitutions
a <+> b =  b ++ a

identity :: Substitutions
identity = []

mapsTo :: Type -> Type -> Substitutions
mapsTo typ sub = [(typ, sub)]

ftv :: Type -> [Type]
ftv t@(TVar id)  = [t]
ftv (TFunc a b)  = (join $ fmap ftv a) ++ ftv b
ftv (TTuple a b) = ftv a ++ ftv b
ftv (TList a)    = ftv a
ftv _            = []

class Substitute a where
    substitute :: Substitutions -> a -> a

instance Substitute Type where
    substitute sub t@(TVar id) = case sub of
        [] -> t
        ((t',s):ss) | t' == t   -> substitute ss s
                    | otherwise -> substitute ss t
    substitute sub (TFunc a b)  = TFunc (fmap (\a' -> (substitute sub a')) a) (substitute sub b)
    substitute sub (TTuple a b) = TTuple (substitute sub a) (substitute sub b)
    substitute sub (TList a)    = TList (substitute sub a)
    substitute _   rest         = rest

instance Substitute SymbolTable where
    substitute sub table =
        let
            entries = assocs table
            mapEntry v@SymbolEntry{..} = v{typing=substitute sub typing}
            newEntries = fmap (\(k,v) -> (k, mapEntry v)) entries
        in
            fromList newEntries

-----------------
-- Unification --
-----------------
unify :: Context -> Type -> Type -> Result Substitutions
unify ctx TInt TInt = pure identity
unify ctx TBool TBool = pure identity
unify ctx TChar TChar = pure identity
unify ctx TVoid TVoid = pure identity

-- unify ctx (TVar "_") b           = pure identity
-- unify ctx a (TVar "_")           = pure identity
unify ctx a@(TVar id) b@(TVar id')
    | id == id' = pure identity
    | otherwise = pure $ a `mapsTo` b
unify ctx a@(TVar id) t
    | a `elem` (ftv t) = Error (freeTypeVarError id ) ctx
    | otherwise = pure $ a `mapsTo` t
unify ctx t a@(TVar id)
    | a `elem` (ftv t) = Error (freeTypeVarError id ) ctx
    | otherwise = pure $ a `mapsTo` t

unify ctx a@(TClass x) b@(TClass y)
    | x == y    = pure identity
    | otherwise = Error (unificationError a b) ctx

unify ctx (TList a) (TList a') = unify ctx a a'

unify ctx (TTuple a b) (TTuple a' b') = do
    sub1 <- unify ctx a a'
    sub2 <- unify ctx (substitute sub1 b) (substitute sub1 b')
    pure $ sub2 <+> sub1

unify ctx f1@(TFunc a b) f2@(TFunc a' b')
    | not (length a == length a') = Error (unificationError f1 f2) ctx
    | otherwise = do
        subs <- Data.List.foldr unifyOverArgs (pure identity) (zip a a')
        subs <- unify ctx (substitute subs b) (substitute subs b')
        pure subs
        where
            unifyOverArgs :: (Type, Type) -> Result Substitutions -> Result Substitutions
            unifyOverArgs (a, b) sub = join $ fmap (\s -> fmap (\s' -> s <+> s') (unify ctx (substitute s a) (substitute s b))) sub
unify ctx a b = Error (unificationError a b) ctx

---------------
-- Inference --
---------------
data InferenceState = InferenceState {
    n       :: Int,
    symbols :: SymbolTable,
    scope   :: String
}

type Inference a = StateT InferenceState Result a
class Infer a where
    infer :: Type -> a -> Inference Substitutions

instance Show InferenceState where show  InferenceState{..}  = "" ++ show n ++ "\n"

updateScope :: String -> StateT InferenceState Result ()
updateScope scope = modify (\i->i{scope=scope})

getScope :: StateT InferenceState Result String
getScope = do
    InferenceState{..} <- get
    pure scope

getSymbols :: StateT InferenceState Result SymbolTable
getSymbols = do
    InferenceState{..} <- get
    pure symbols

tryUnify ::  Substitutions -> Context -> Type -> Type -> Inference Substitutions
tryUnify sub1 ctx t1 t2 = do
    sub2 <- lift $ unify ctx (substitute sub1 t1) (substitute sub1 t2)
    return $ sub2 <+> sub1

return :: Substitutions -> Inference Substitutions
return sub = do
    symbols <- getSymbols

    modify (\i->i{symbols=substitute sub symbols})
    pure sub

getType :: String -> Context -> Inference Type
getType id ctx  =
    let
        maybeToResult (Just x@SymbolEntry{..}) id ctx = pure typing
        maybeToResult Nothing id ctx = Error (undeclaredVar id) ctx
    in
        do
        scope <- getScope
        symbols <- getSymbols

        let mt =  (lookup id symbols)
        t <- lift $ maybeToResult mt id ctx
        pure t

setType :: String -> String -> Type -> Context -> Inference ()
setType scope name t ctx  =
    let
        maybeToResult (Just x) id ctx = pure x
        maybeToResult Nothing id ctx  = Error (undeclaredVar id) ctx
        id = idOf scope name
    in
        do
        scope <- getScope
        symbols <- getSymbols


        let maybeEntry =  lookup id symbols
        entry <- lift $ maybeToResult maybeEntry id ctx
        let entry' = entry{typing=t}
        let symbols' = addSymbol scope name entry' symbols

        modify (\i->i{symbols=symbols'})

fresh :: Inference Type
fresh = do
    InferenceState{..} <- get
    modify (\i -> i{n=n+1})
    pure $ TVar (identifiers !! n)
    where
        identifiers = ["_" ++ replicate cnt v | cnt <- [1..], v <- ['a'..'z']]

instance Infer Exp where
    infer typing x@(VarExp id ctx) = do
        scope <- getScope

        t <- getType (idOf scope id) ctx
        sub <- tryUnify identity ctx t typing

        return sub

    infer typing x@(IntExp _ ctx) = do
        sub <- tryUnify identity ctx TInt typing
        return sub

    infer typing x@(CharExp _ ctx) = do
        sub <- tryUnify identity ctx TChar typing
        return sub

    infer typing x@(BoolExp _ ctx)  = do
        sub <- tryUnify identity ctx TBool typing
        return sub

    infer typing x@(EmptyList ctx) = pure identity

    infer typing x@(UnExp op e ctx) = do
        let (argT, retT) = unaryOpToType op

        a <- fresh
        sub <- infer argT e
        sub <- tryUnify sub ctx retT typing

        return sub

    infer typing bin@(BinExp Cons e1 e2 ctx) = do
        a <- fresh

        sub1 <- infer a                           e1
        sub2 <- infer (TList (substitute sub1 a)) e2

        let sub = sub2 <+> sub1
        sub <- tryUnify sub ctx (TList (substitute sub a)) typing

        return sub

    infer typing bin@(BinExp op e1 e2 ctx) =
        let
            infer' typing bin@(BinExp op e1 e2 ctx) (argT1, argT2, retT) = do
                sub1 <- infer argT1 e1
                sub2 <- infer argT2 e2
                let sub = sub2 <+> sub1

                sub <- tryUnify sub ctx retT typing

                return sub
        in
            do
                let (argT1, argT2, retT) = binOpToType op
                a <- fresh
                case op of
                    Eq        -> infer' typing bin (a, a, retT)
                    NEq       -> infer' typing bin (a, a, retT)
                    otherwise -> infer' typing bin (argT1, argT2, retT)

    infer typing (TupleExp e1 e2 ctx) = do
        symbols <- getSymbols
        a1 <- fresh
        a2 <- fresh

        sub1 <- infer a1 e1
        sub2 <- infer a2 e2

        let sub = sub2 <+> sub1
        sub <- tryUnify sub ctx (TTuple a1 a2) typing

        return sub

    infer typing (FunCallExp "print" args ctx) = do
        a <- fresh
        sub <- infer a args
        return sub

    infer typing (FunCallExp id args ctx)  =
        let
            getArgTypes (TFunc argTypes retType) = argTypes
            getRetType (TFunc argTypes retType) = retType

            inferArgs ::  [Type] -> [Exp] -> Context -> Inference Substitutions
            inferArgs [] [] ctx = pure identity
            inferArgs ts as ctx
                | not (length ts == length as) = lift $ Error (argumentLengthError (length ts) (length as)) ctx
                | otherwise = do
                    let (typ:types) = ts
                    let (arg:args)  = as

                    a <- fresh

                    -- Infer type of expression
                    sub1 <- infer a arg
                    sub2 <- inferArgs types args ctx
                    let sub = sub2 <+> sub1

                    -- Unify argument type with declared argument type
                    sub <- tryUnify sub (expCtx arg) a typ

                    return sub
        in
            do
                scope <- getScope

                t <- getType (idOf scope id) ctx
                let argTypes  = getArgTypes t
                let retType   = getRetType t

                sub <- inferArgs argTypes args ctx
                sub <- tryUnify sub ctx retType typing

                symbols <- getSymbols

                let typeVars =  [ x | x@(TVar _) <- retType : argTypes]
                let p a = not (a `elem` typeVars)
                let sub' = [ (a,b) | (a,b) <- sub, p a] :: Substitutions

                setType scope id t ctx

                return sub'


instance Infer Stmt where
    infer typing (If exp stmt els)  =
        let
            maybeToList Nothing   = []
            maybeToList (Just xs) = xs
        in
            do
                let elseStmts = maybeToList els

                sub1 <- infer TBool exp
                sub2 <- infer TVoid (stmt ++ (   elseStmts))

                let sub =  sub2 <+> sub1
                return sub

    infer typing (While exp stmts) = do
        sub1 <- infer TBool exp
        sub2 <- infer TVoid stmts

        let sub = sub2 <+> sub1
        return sub

    infer typing (Assign id exp ctx) = do
        scope <- getScope
        symbols <- getSymbols

        a <- fresh
        sub <- infer a exp

        t <- getType (idOf scope id) ctx
        sub <- tryUnify sub ctx t a

        return sub

    infer typing (FunCallStmt i exps ctx) = do
        a <- fresh
        -- hack
        sub <- infer a (FunCallExp i exps ctx)

        return sub

    infer typing (Return Nothing ctx) =
        let
            getRetType (TFunc argTypes retType) = retType
        in
            do
                scope <- getScope -- Scope = function name

                t <- getType (idOf "*" scope) ctx
                let retType = getRetType t

                sub <- tryUnify identity ctx retType TVoid
                return sub

    infer typing (Return (Just e) ctx) =
        let
            getRetType (TFunc argTypes retType) = retType
        in
            do
                scope <- getScope -- Scope = function name

                t <- getType (idOf "*" scope) ctx
                let retType = getRetType t

                a <- fresh
                sub <- infer a e
                sub <- tryUnify sub ctx retType a

                return sub

instance Infer VarDecl where
    infer typing (VarDecl v id exp ctx)  = do
        sub <- infer typing (Assign id exp ctx)
        return sub

instance Infer FunDecl where
    infer typing (FunDecl id idArgs (TFunc tArgs t) vars stmts ctx) =  do
        scope <-  getScope

        -- Set scope to function
        updateScope  id

        sub1 <- infer TVoid vars
        sub2 <- infer TVoid stmts

        -- Reset scope
        updateScope scope

        let sub = sub2 <+> sub1
        return sub

instance Infer SPL where

    infer typing z@(SPL cs (v:vars) fs)  = do
        sub1 <- infer typing  v
        sub2 <- infer typing (SPL cs vars fs)

        let sub = sub2 <+> sub1
        return sub

    infer typing z@(SPL cs vars (f:fs)) = do
        sub1 <-  infer typing f
        sub2 <-  infer typing (SPL cs vars fs)

        let sub = sub2 <+> sub1
        return sub

    infer typing _ = pure identity

instance Infer a => Infer [a] where
    infer t [] = pure identity
    infer t (x:xs)  = do
        sub1 <- infer t x
        sub2 <- infer t xs

        let sub = sub2 <+> sub1
        return sub

typeOf :: String -> SymbolTable -> Exp -> Type
typeOf scope table (IntExp _ _) = TInt
typeOf scope table (CharExp _ _) = TChar
typeOf scope table (BoolExp _ _) = TBool
typeOf scope table (EmptyList _) = TList Untyped
typeOf scope table (TupleExp e1 e2 _) = TTuple (typeOf scope table e1) (typeOf scope table e2)
typeOf scope table (UnExp op _ _) = case unaryOpToType op of (_, t) -> t
typeOf scope table (BinExp op _ _ _) = case binOpToType op of (_, _, t) -> t
typeOf scope table (FunCallExp id _ _) =
    case lookup (idOf scope id) table of
        Just (SymbolEntry{..}) ->
            case typing of TFunc _ t -> t
typeOf scope table (VarExp id _) =
    case lookup (idOf scope id) table of
        Just (SymbolEntry{..}) -> typing

unaryOpToType :: UnaryOp -> (Type, Type)
unaryOpToType Neg = (TBool, TBool)
unaryOpToType Min = (TInt, TInt)

binOpToType :: BinaryOp -> (Type, Type, Type)
binOpToType Add  = (TInt, TInt, TInt)
binOpToType Sub  = (TInt, TInt, TInt)
binOpToType Mult = (TInt, TInt, TInt)
binOpToType Div  = (TInt, TInt, TInt)
binOpToType Mod  = (TInt, TInt, TInt)
binOpToType Lt   = (TInt, TInt, TBool)
binOpToType Gt   = (TInt, TInt, TBool)
binOpToType LtEq = (TInt, TInt, TBool)
binOpToType GtEq = (TInt, TInt, TBool)
binOpToType And  = (TBool, TBool, TBool)
binOpToType Or   = (TBool, TBool, TBool)
binOpToType Eq   = (TInt, TInt, TBool)
binOpToType NEq  = (TInt, TInt, TBool)
binOpToType Cons = (TInt, TInt, TBool)
