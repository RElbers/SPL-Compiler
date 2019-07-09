{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Shared.AST where

import           Control.Monad
import           Data.List
import           Shared.Result

-- AST
data SPL
    =  SPL [ClassDecl] [VarDecl] [FunDecl]
    deriving (Show, Eq, Ord)
data ClassDecl
    = ClassDecl String [Attribute] Context
    deriving (Show, Eq, Ord)
data Attribute
    = Attribute String Type Context
    deriving (Show, Eq, Ord)
data VarDecl
    = VarDecl Type String Exp Context
    deriving (Show, Eq, Ord)
data FunDecl
    = FunDecl String [String] Type [VarDecl] [Stmt] Context
    deriving (Show, Eq, Ord)
data Type
    = Untyped
    | TClass String
    | TInt
    | TBool
    | TChar
    | TVoid
    | TTuple Type Type
    | TList Type
    | TVar String
    | TFunc [Type] Type
    deriving (Eq, Ord)
data Stmt
    = If Exp [Stmt] (Maybe [Stmt])
    | While Exp [Stmt]
    | Assign String Exp Context
    | FunCallStmt String [Exp] Context
    | Return (Maybe Exp) Context
    deriving (Show, Eq, Ord)
data Exp
    = VarExp String Context
    | UnExp UnaryOp Exp Context
    | IntExp Int Context
    | CharExp Char Context
    | BoolExp Bool Context
    | EmptyList Context
    | FunCallExp String [Exp] Context
    | TupleExp Exp Exp Context
    | BinExp BinaryOp Exp Exp Context
    deriving (Show, Eq, Ord)
data UnaryOp
    = Neg
    | Min
    deriving (Show, Eq, Ord)
data BinaryOp
    = Add
    | Sub
    | Mult
    | Div
    | Mod
    | Eq
    | Lt
    | Gt
    | LtEq
    | GtEq
    | NEq
    | And
    | Or
    | Cons
    | App
    deriving (Show, Eq, Ord)

data Field
    = Field String Context
    deriving (Show, Eq, Ord)
-------------------------
-- Operator precedence --
-------------------------
hack f = \c a b -> f a b c
opPrecedenceRight = [
        [(":", hack $ BinExp Cons)]
    ]

opPrecedenceLeft = [
    [("+", hack $ BinExp Add),
    ("-", hack $ BinExp Sub)],

    [("*", hack $ BinExp Mult),
    ("/", hack $ BinExp Div),

    ("%", hack $ BinExp Mod)],

    [("<=", hack $ BinExp LtEq),
    (">=", hack $ BinExp GtEq),
    ("<", hack $ BinExp Lt),
    (">", hack $ BinExp Gt)],

    [("==", hack $ BinExp Eq),
    ("!=", hack $ BinExp NEq)],

    [("&&", hack $ BinExp And)],

    [("||", hack $ BinExp Or)],
    [(".", doApp)]

    ]
    where
        doApp :: Context -> Exp -> Exp -> Exp
        doApp ctx lhs (FunCallExp id args ctx') = FunCallExp id (lhs:args) ctx
        doApp ctx lhs (VarExp id ctx') = FunCallExp (getter id) [lhs] ctx
        doApp ctx lhs rhs = FunCallExp "app" [lhs] ctx




----------
-- Util --
----------
class Accessors a where
    getter :: a -> String
    setter :: a -> String

instance Accessors String where
    getter n =  "_get_" ++ n
    setter n =  "_set_" ++ n

instance Accessors Attribute where
    getter (Attribute n _ _) = getter n
    setter (Attribute n _ _) = setter n

instance Accessors Field where
    getter (Field n _) = getter n
    setter (Field n _) = setter n

expCtx:: Exp -> Context
expCtx (VarExp _ ctx)       = ctx
expCtx (UnExp _ _ ctx)      = ctx
expCtx (IntExp _ ctx)       = ctx
expCtx (CharExp _ ctx)      = ctx
expCtx (BoolExp _ ctx)      = ctx
expCtx (EmptyList ctx)      = ctx
expCtx (FunCallExp _ _ ctx) = ctx
expCtx (TupleExp _ _ ctx)   = ctx
expCtx (BinExp _ _ _  ctx)  = ctx


instance Show Type where
    show Untyped        = "var"
    show (TClass i)     = i
    show TInt           = "Int"
    show TBool          = "Bool"
    show TChar          = "Char"
    show TVoid          = "Void"
    show (TTuple t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
    show (TList t)      = "[" ++ show t ++ "]"
    show (TVar tv)      = tv
    show (TFunc argsT t2)  =
        let
            args = fmap show argsT
            args' = intersperse " "  args
        in
               (join args') ++ " -> " ++ show t2


classCtorType (ClassDecl name attrs _) = TFunc (fmap attrType attrs) (TClass name)
attrType (Attribute _ typing _) = typing
attrGetterType className (Attribute _ typing _) = TFunc [(TClass className)] typing
attrSetterType className (Attribute _ typing _) = TFunc [(TClass className), typing] (TClass className)
