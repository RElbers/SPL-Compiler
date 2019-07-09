{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module CodeGen.CodeGenerator where

import           CodeGen.IntermediateLanguage
import           Control.Monad.Trans.State
import           Data.Char                    (ord)
import           Data.List                    hiding (insert, lookup)
import           Data.Map                     hiding (foldr, lookup, show)
import qualified Data.Map                     (lookup)
import           Data.Maybe
import           Prelude                      hiding (EQ, GT, LT, lookup)
import           Shared.AST
import           Shared.Result
import           Shared.SymbolTable
import           Typing.TypeChecker           (typeOf)

codeGen :: Emit a => SymbolTable -> a -> String
codeGen symbols a =
  let
      generator = populateAccessors >> emit a :: SSMGenerator [IL]
      program = evalState generator (initState symbols) :: [IL]
      strings = serialize program
   in
    strings

populateAccessors :: SSMGenerator ()
populateAccessors  =
  let
    populate [] = pure ()
    populate (((scope, name), entry):xs) = addAccessors scope name entry >> populate xs
  in
    do
      symbols <- state getSymbols
      let entries = assocs symbols
      populate entries

-----------------
-- State monad --
-----------------
data SSMState = SSMState
  { labelIdx :: Int
  , scope    :: [String]
  , symbols  :: SymbolTable
  , setters  :: Map String (SSMGenerator [IL])
  , getters  :: Map String (SSMGenerator [IL])
  }

-- Getters
getLabelIdx state@SSMState {..} = (labelIdx, state)
getSymbols state@SSMState {..} = (symbols, state)
getScope state@SSMState {..} = (scope, state)
getGetters state@SSMState {..} = (getters, state)
getSetters state@SSMState {..} = (setters, state)

-- Initial state
initState symbols =
  SSMState
    { labelIdx = 0
    , symbols = symbols
    , scope = ["*"]
    , setters = empty
    , getters = empty

    }

-- Show for debugging
instance Show SSMState where
  show SSMState {..} =
    let
      printMap map =
        let
          entries = assocs map
          strs = fmap (\(a,b) -> a) entries
        in
          show $ intercalate "\n" strs

      printArgs map =
        let
          entries = assocs map
          strs = fmap (\((a, b),_) -> a++"_"++b) entries
        in
          show $ intercalate "\n" strs
    in
      "labelIdx: " ++ show labelIdx ++ " | " ++
      "scope: " ++ show scope ++ " | "

------------------------
-- SSMGenerator Monad --
------------------------
infixr 4 <+>
infixr 4 +>
infixr 4 <+
type SSMGenerator a = State SSMState a
instance Show (SSMGenerator a) where show x = "SSMGenerator"

-- Combinator for combining generators, keeping both results
(<+>) :: SSMGenerator [a] -> SSMGenerator [a] -> SSMGenerator [a]
a <+> b = (++) <$> a <*> b

-- Forget left generator
(+>) :: SSMGenerator a -> SSMGenerator b -> SSMGenerator b
a +> b = (\_ y -> y) <$> a <*> b

-- Forget right generator
(<+) :: SSMGenerator a -> SSMGenerator b -> SSMGenerator a
a <+ b = (\x _ -> x) <$> a <*> b

-- Returns the next ID for creating unique labels
nextId :: SSMGenerator String
nextId =
  let identifiers = [replicate cnt v | cnt <- [1 ..], v <- ['A' .. 'Z']] :: [String]
    in state $ \s@SSMState {..} -> (identifiers !! labelIdx, s {labelIdx = labelIdx + 1})


initializeSymbol :: String -> SSMGenerator [IL]
initializeSymbol name = do
  scope <- state peekScope
  symbols <- state getSymbols
  case lookup (idOf scope name) symbols of
    Just e@SymbolEntry{..} -> case sort of
      Local    -> getSetter name
      Global   -> emit STH
      Argument -> pure []
    Nothing -> unexpectedSymbol (idOf scope name)

-- Gets IL for loading the variable
addAccessors :: String -> String -> SymbolEntry -> SSMGenerator ()
addAccessors scope name' e@SymbolEntry{..} = do
  case sort of
    Local    -> local e
    Global   -> global e
    Argument -> arg e
  where
    id = idOf scope name'

    local SymbolEntry{..} =
      let
        getter = emit (LDL (idx+1))
        setter = emit (STL (idx+1))
        updateState state@SSMState {..} =
          ((), state {
            getters=insert id getter getters,
            setters=insert id setter setters})
      in
        do
          state updateState

    arg SymbolEntry{..} =
      let
        getter = emit (LDL (-(idx+2)))
        setter = emit (STL (-(idx+2)))
        updateState state@SSMState {..} =
          ((), state {
            getters=insert id getter getters,
            setters=insert id setter setters})
       in
        do
          state updateState

    global SymbolEntry{..} =
      let
        getter = emit (LDR R5) <+> emit (LDC idx) <+> emit ADD <+> emit (LDA 0)
        setter = emit (LDR R5) <+> emit (LDC idx) <+> emit ADD <+> emit (STA 0)
        updateState state@SSMState {..} =
          ((), state {
            getters=insert id getter getters,
            setters=insert id setter setters})
      in
        do
          state updateState

getGetter :: String -> SSMGenerator [IL]
getGetter name = do
  scope <- state peekScope
  getters <- state getGetters

  case Data.Map.lookup (idOf scope name) getters of
    Just get -> get
    Nothing -> case Data.Map.lookup (idOf "*" name) getters of
      Just get -> get
      Nothing  -> unexpectedSymbol (idOf scope name)

getSetter :: String -> SSMGenerator [IL]
getSetter name = do
  scope <- state peekScope
  setters <- state getSetters

  case Data.Map.lookup (idOf scope name) setters of
    Just set -> set
    Nothing -> case Data.Map.lookup (idOf "*" name) setters of
      Just set -> set
      Nothing  -> unexpectedSymbol (idOf scope name)


-- Manipulate scope
peekScope  state@SSMState {..} = (head scope, state)
popScope state@SSMState {..} = ((), state{scope=tail scope})
pushScope newScope state@SSMState {..} = ((), state{scope=newScope:scope})

---------------------
-- Code generation --
---------------------

-- Emit IL for type
class Emit a where
  emit :: a -> SSMGenerator [IL]

instance Emit a => Emit [a] where
  emit []     = return []
  emit (a:as) = (++) <$> emit a <*> emit as

instance Emit a => Emit (Maybe a) where
  emit Nothing  = return []
  emit (Just a) = emit a

instance Emit Instruction where
  emit i = (:) <$> pure (instruction i) <*> pure []

instance Emit IL where
  emit i = (:) <$> pure i <*> pure []

--------------------
-- Emit Instances --
--------------------

instance Emit SPL where
  emit (SPL classes globals functions) =
      emit (LDR HP) <+> -- Store Heap start to R5
      emit (STR R5) <+>
      emit globals <+>
      emit (BSR "main") <+>
      emit HALT <+>
      emit classes <+>
      emit functions

instance Emit ClassDecl where
  -- Emit ctor, emit args
  emit (ClassDecl name attrs _) = do
    -- Constructor
    emit (label name) <+> emit (LINK (length attrs)) <+>
      state (pushScope name) +>
      (foldr (\(Attribute n _ _)  il -> getGetter n <+> il) (pure []) attrs) <+>
      state popScope +>
      emit (STMH (length attrs)) <+>
      emit (STR RR) <+>
      emit UNLINK <+>
      emit (STS (-1))<+>
      emit RET <+>
      emit (fmap (\a -> (a, name)) attrs)

instance Emit (Attribute, String) where
  emit (a@(Attribute name _ _), className) = getterIL <+> setterIL
    where
      attributeIdx symbols = case lookup (idOf className name) symbols of Just SymbolEntry{..} -> idx

      getterIL = do
        symbols <- state getSymbols
        let idx = attributeIdx symbols

        emit (label (getter a)) <+> emit (LINK 0) <+>
          state (pushScope (getter name)) +>
          getGetter "this" <+>  -- Load argument
          state popScope +>
          emit (LDH (-idx)) <+>  -- Offset
          emit (STR RR) <+>
          emit UNLINK <+>
          emit (STS (-1)) <+>
          emit RET

      setterIL = do
        symbols <- state getSymbols
        let idx = attributeIdx symbols

        emit (label (setter a)) <+> emit (LINK 0) <+>
          state (pushScope (setter name)) +>
          getGetter "val" <+>  -- Load argument
          getGetter "this" <+>  -- Load argument
          state popScope +>
          emit (LDS (-1)) <+>
          emit (STA (-(idx))) <+>
          emit (STR RR) <+>
          emit UNLINK <+>
          emit (STS (-1)) <+>
          emit RET


instance Emit VarDecl where
  emit (VarDecl t id exp _) = do emit exp <+> initializeSymbol id

instance Emit FunDecl where
  emit (FunDecl id args t vars stmts _) =
      state (pushScope id) +>
        emit (label id) <+> emit (LINK (length vars)) <+> emit vars <+> emit stmts <+
        state popScope

instance Emit Stmt where
  emit (If guard stmts stmtsElse) = do
    scope <- state peekScope
    tag <- nextId
    trueTag <- pure $ scope ++  "True" ++ tag
    falseTag <- pure $ scope ++ "False" ++ tag
    endTag <- pure $ scope ++ "End" ++ tag

    emit guard <+>
      emit (BRF falseTag) <+>
      emit (label trueTag) <+>
      emit stmts <+>
      emit (BRF endTag) <+>
      emit (label falseTag) <+>
      emit stmtsElse <+>
      emit (label endTag)

  emit (While guard stmts) = do
    scope <- state peekScope
    tag <- nextId
    testTag <- pure $ scope ++ "Test" ++ tag
    endTag <- pure $ scope ++ "End" ++ tag

    emit (label testTag) <+>
      emit guard <+>
      emit (BRF endTag) <+>
      emit stmts <+>
      emit (BRA testTag) <+>
      emit (label endTag)

  emit (Assign "_" exp _) = emit exp
  emit (Assign id exp _) = emit exp <+> getSetter id

  emit (FunCallStmt "print" [] _)     = pure []
  emit (FunCallStmt "print" (e:es) _) = do
    scope <- state peekScope
    symbols <- state getSymbols
    let t = typeOf scope symbols e
    case t of
      TChar -> emit e <+> emit (TRAP 1) <+> emit (FunCallStmt "print" es eof)
      otherwise -> emit e <+> emit (TRAP 0) <+> emit (FunCallStmt "print" es eof)

  emit (FunCallStmt id args _)         = emit args <+> emit (BSR id)

  emit (Return Nothing _) =
    emit UNLINK <+>
    emit RET
  emit (Return exp _) =
    emit exp <+>
    emit (STR RR) <+>
    emit UNLINK <+>
    emit (STS (-1))<+>
    emit RET

instance Emit Exp where
  emit (FunCallExp  "read" args _)     = emit (TRAP 10)
  emit (FunCallExp "_get_fst" args _)  = emit args <+> emit (LDH (-1))
  emit (FunCallExp "_get_snd" args _)  = emit args <+> emit (LDH (0))
  emit (FunCallExp "_get_hd" args _)   = emit args <+> emit (LDH (-1))
  emit (FunCallExp "_get_tl" args _)   = emit args <+> emit (LDH (0))

  emit (FunCallExp "_set_fst" [listPtr,val] _)  = emit listPtr <+> emit val <+> emit (LDS (-1)) <+> emit (STA (-1))
  emit (FunCallExp "_set_snd" [listPtr,val] _)  = emit listPtr <+> emit val <+> emit (LDS (-1)) <+> emit (STA (0))
  emit (FunCallExp "_set_hd" [listPtr,val] _)   = emit listPtr <+> emit val <+> emit (LDS (-1)) <+> emit (STA (-1))
  emit (FunCallExp "_set_tl" [listPtr,val] _)   = emit listPtr <+> emit val <+> emit (LDS (-1)) <+> emit (STA (0))

  emit (FunCallExp "isEmpty" [e] _) = emit (BinExp Eq (IntExp 0 eof) e eof)

  emit (VarExp id _)               = getGetter id
  emit (IntExp i _)                = emit (LDC i)
  emit (CharExp c _)               = emit (LDC (ord c))
  emit (BoolExp True _)            = emit (LDC (-1))
  emit (BoolExp False _)           = emit (LDC 0)
  emit (UnExp op exp _)            = emit exp <+> emit op
  emit (TupleExp e1 e2 _)          = emit e1 <+> emit e2 <+> emit (STMH 2)
  emit (EmptyList _)               = emit (LDC 0)
  emit (FunCallExp id args c)      = emit (FunCallStmt id args c) <+> emit (LDR RR)
  emit (BinExp op e1 e2 _)         = emit e1 <+> emit e2 <+> emit op

instance Emit UnaryOp where
  emit Neg = emit (LDC true) <+> emit XOR
  emit Min = emit NEG

instance Emit BinaryOp where
  emit Add  = emit ADD
  emit Sub  = emit SUB
  emit Mult = emit MUL
  emit Div  = emit DIV
  emit Mod  = emit MOD
  emit Eq   = emit EQ
  emit NEq  = emit NE
  emit Lt   = emit LT
  emit Gt   = emit GT
  emit LtEq = emit LE
  emit GtEq = emit GE
  emit And  = emit AND
  emit Or   = emit OR
  emit Cons = emit (STMH 2)

unexpectedSymbol s = undefined
