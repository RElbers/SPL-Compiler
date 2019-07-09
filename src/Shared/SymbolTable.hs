{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Shared.SymbolTable where

import           Data.Map      hiding (foldr)
import           Prelude       hiding (lookup)
import           Shared.AST
import           Shared.Result

buildSymbolTable :: SPL -> SymbolTable
buildSymbolTable spl =
    let
        table = (build "*" 0 spl)
    in
        updateTypeVars (union initial table)

-----------------
-- SymbolTable --
-----------------
data VariableSort = Global | Argument | Local
    deriving (Show)

type SymbolTable = Map (String, String) SymbolEntry

data SymbolEntry = SymbolEntry{
    name   :: String,
    idx    :: Int,
    typing :: Type,
    sort   :: VariableSort,
    loc    :: Context
}

instance Show SymbolEntry where
    show SymbolEntry{..} =
        "\n{ name: " ++ show name ++ " \n" ++
        "  idx: " ++  show idx ++ " \n" ++
        "  typing: " ++ show typing ++ " \n" ++
        "  sort: " ++ show sort ++ " } \n\n"

entry = SymbolEntry
        {
        name   = "*",
        idx    = 0,
        typing = Untyped,
        sort   = Global,
        loc    = eof
        }

lookup :: String -> SymbolTable -> Maybe SymbolEntry
lookup id table =
    let
        splitOn :: (a -> Bool) -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn f l@(x:xs)
            | f x = splitOn f xs
            | otherwise = let (h,t) = break f l in h:(splitOn f t)

        split = splitOn ((==) '^') id
        scope = head split :: String
        name = head $ tail split :: String
        local = Data.Map.lookup (scope, name) table
        global = Data.Map.lookup ("*", name) table
    in
        case local of
            Just e -> Just e
            Nothing -> case global of
                Just e  -> Just e
                Nothing -> Nothing


addSymbol :: String -> String -> SymbolEntry -> SymbolTable -> SymbolTable
addSymbol scope name symbol table = insert (scope, name) symbol table

---------------------
-- Building tables --
---------------------
idOf scope name =  '^':scope ++ "^" ++ name
    where identifiers = [replicate cnt v | cnt <- [1 ..], v <- ['a' .. 'z']] :: [String]

updateTypeVars :: SymbolTable -> SymbolTable
updateTypeVars table =
    let
        instantiate (Untyped) s n = TVar (idOf s n) -- Create unique name
        instantiate (TTuple t1 t2) s n = TTuple (instantiate t1 s n) (instantiate t2 s n)
        instantiate (TList t) s n = TList (instantiate t s n)
        instantiate (TVar k) s n = TVar ((idOf s n) ++ "_" ++ k)
        instantiate (TFunc args ret) s n = TFunc (fmap (\a -> instantiate a s n) args) (instantiate ret s n)
        instantiate t s n = t

        entries = assocs table
        f (k@(s, n), e@SymbolEntry{..}) = (k, e{typing=instantiate typing s n})
        newEntries = fmap f entries
    in
        fromList newEntries

class BuildSymbolTable a where
    -- Scope, Idx, 'thing' -> Symbol Table
    build :: String -> Int -> a -> SymbolTable

instance BuildSymbolTable a => BuildSymbolTable [a] where
    build _ _ []       = empty
    build s idx (x:xs) = union (build s idx x) (build s (idx+1) xs)

instance BuildSymbolTable SPL where
    build s idx (SPL classes vars funcs) =
        let
            classTable = build s idx classes
            varTable = build s idx vars
            funcTable = build s idx funcs
            table =  union classTable (union varTable funcTable)
        in
            table

instance BuildSymbolTable Attribute where
    build className idx a@(Attribute name typing ctx) =
        let
            getterF = FunDecl (getter a) ["this"] (TFunc [(TClass className)] typing) [] [] ctx
            setterF = FunDecl (setter a) ["this", "val"] (TFunc [(TClass className), typing] (TClass className)) [] [] ctx

        in
            build name 0 [getterF, setterF]

instance BuildSymbolTable ClassDecl where
    build s idx c@(ClassDecl name attrs ctx) =
        let
            argTypes = fmap (\a@(Attribute _ t _) -> t) attrs
            argNames = fmap (\a@(Attribute n _ _) -> n) attrs
            ctorF = FunDecl name argNames (TFunc argTypes (TClass name)) [] [] ctx
            ctorEntry = build s 0 ctorF

            attrsEntries = build name 0 attrs
        in
            union attrsEntries ctorEntry

instance BuildSymbolTable VarDecl where
    build s idx (VarDecl var id exp ctx) =
        let
            sort = case s of
                "*"       -> Global
                otherwise -> Local
            varEntry = entry{name=id, sort=sort, idx=idx, loc=ctx}
        in
            addSymbol s id varEntry empty

instance BuildSymbolTable FunDecl where
    build s idx (FunDecl id (args) (TFunc argTypes retType) locals stmts ctx) =
        let
            funcEntry = entry{
                name=id,
                idx=idx,
                sort=Global,
                typing=TFunc argTypes retType,
                loc=ctx}
            funcTable = addSymbol "*" id funcEntry empty

            argEntry (idx, (id, t))= entry{
                name=id,
                idx=idx,
                sort=Argument,
                typing=t,
                loc=ctx}
            argEntries = fmap argEntry (zip [0..] (zip args argTypes))
            argTables [] = []
            argTables (e@SymbolEntry{..}:es) = addSymbol id name e empty:argTables es

            localEntry (idx, (VarDecl t id exp ctx))= entry{
                name=id,
                idx=idx,
                sort=Local,
                typing=t,
                loc=ctx}
            localEntries = fmap localEntry (zip [0..] locals)
            localTables [] = []
            localTables (e@SymbolEntry{..}:es) = addSymbol id name e empty:localTables es

            allTables =  funcTable:((argTables argEntries) ++ (localTables localEntries))
        in
            foldr (\a b -> union a b) empty allTables

-------------------
-- Initial table --
-------------------
initial :: SymbolTable
initial =
    let
        getFirst = entry{name="_get_fst",   typing=TFunc [TTuple (TVar "a") (TVar "b")] (TVar "a")}
        getSecond = entry{name="_get_snd",  typing=TFunc [TTuple (TVar "a") (TVar "b")] (TVar "b")}
        getHead = entry{name="_get_hd",     typing=TFunc [TList (TVar "a")] (TVar "a")}
        getTail = entry{name="_get_tl",     typing=TFunc [TList (TVar "a")] (TList (TVar "a"))}

        setFirst = entry{name="_set_fst",   typing=TFunc [TTuple (TVar "a") (TVar "b"), TVar "a"] (TTuple (TVar "a") (TVar "b"))}
        setSecond = entry{name="_set_snd",  typing=TFunc [TTuple (TVar "a") (TVar "b"), TVar "b"] (TTuple (TVar "a") (TVar "b"))}
        setHead = entry{name="_set_hd",     typing=TFunc [TList (TVar "a"), TVar "a"] (TList (TVar "a"))}
        setTail = entry{name="_set_tl",     typing=TFunc [TList (TVar "a"), TList (TVar "a")] (TList (TVar "a"))}

        print = entry{name="print",      typing=TFunc [(TVar "a")] TVoid}
        isEmpty = entry{name="isEmpty",  typing=TFunc [TList (TVar "a")] TBool}
        read = entry{name="read",        typing=TFunc [] (TList TChar)}

        garbage = entry{name="_",        typing=TVar "_"}

        entries = [
            getFirst,
            setFirst,
            getSecond,
            setSecond,
            getHead,
            setHead,
            getTail,
            setTail,
            print,
            isEmpty,
            read,
            garbage
            ]
        tables []                     = []
        tables (e@SymbolEntry{..}:es) = addSymbol "*" name e empty:tables es
    in
        foldr (\a b -> union a b) empty (tables entries)
