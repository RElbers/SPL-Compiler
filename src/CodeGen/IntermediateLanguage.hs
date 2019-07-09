module CodeGen.IntermediateLanguage where

import           Prelude hiding (EQ, GT, LT)

instruction :: Instruction -> IL
instruction i = IL {i=i, lbl=[], annote=[]}

label :: String -> IL
label lbl = IL {i=NOP, lbl=lbl, annote=[]}

comment :: String -> IL
comment str = IL {i=NOP, lbl=[], annote=str}

data IL = IL
  { i      :: Instruction
  , lbl    :: String
  , annote :: String
  } deriving (Show, Eq, Ord)

data Register = HP | RR | R5
  deriving (Show, Eq, Ord)

true :: Int
true = 2^32-1
false :: Int
false = 0

data Instruction
  = NOP
  | HALT
  -- Load
  | LDH Int --load a value relative to the HP.
  | LDMH Int  Int
  | LDC Int --load a constant.
  | LDS Int --load a value relative to the SP.
  | LDL Int --load a value relative to the MP.
  | LDA Int --load a value pointed to by the value on top of the stack.
  | LDR Register --load a register value.
  | LDRR Register Register --load a register with a value from another register.
  | LDSA Int --load address of value relative to the SP.
  | LDLA Int --load address of value relative to the MP.
  | LDAA Int --load address of value relative to the address on top of the stack.
  -- Store
  | STS Int --store a value relative to the SP.
  | STL Int --store a value relative to the MP
  | STA Int -- Store via Address.
  | STR Register --store a value in a register.
  | STH  --store a value relative to the HP.
  | STMH Int -- Store Multiple into Heap.

  | AJS Int --adjust the SP.
  | LINK Int --save MP, adjust MP and SP suitable for programming language function entry.
  | UNLINK --reverse of link
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | NEG
  | AND
  | OR
  | XOR
  | NOT
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | BEQ String --Branch based on condition.
  | BNE String
  | BLT String
  | BGT String
  | BLE String
  | BGE String
  | BRA String --branch always, no popping of the stack.
  | BRF String --branch if top of stack is false .
  | BRT String --branch if top of stack is true.
  | BSR String --branch to subroutine. Like bra, but pushes the previous PC before jumping.
  | JSR --jump to subroutine. Like bsr, but pops its destination from the stack.
  | RET --return
  | TRAP Int -- printing
  deriving (Show, Eq, Ord)


class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable [a] where
  serialize []     = []
  serialize (a:as) = serialize a ++ " \n" ++ serialize as

instance Serializable IL where
  serialize IL{i=i, lbl=[], annote=c} = "    " ++ serialize i ++ " ; " ++ c
  serialize IL{i=i, lbl=s, annote=c}  =  s ++ ": " ++ serialize i ++ " ; " ++ c

instance Serializable Register where
  serialize HP = "HP"
  serialize RR = "RR"
  serialize R5 = "R5"

instance Serializable Instruction where
  serialize (NOP)        = "nop"
  serialize (HALT)       = "halt"
  serialize (STH)        = "sth"
  serialize (LDH i)      = "ldh" ++ " " ++ show i
  serialize (LDMH i1 i2) = "ldmh" ++ " " ++ show i1 ++ " " ++ show i2
  serialize (LDC i)      = "ldc" ++ " " ++ show i
  serialize (LDS i)      = "lds" ++ " " ++ show i
  serialize (LDL i)      = "ldl" ++ " " ++ show i
  serialize (LDA i)      = "lda" ++ " " ++ show i
  serialize (LDR r)      = "ldr" ++ " " ++ serialize r
  serialize (LDRR r1 r2) = "ldrr" ++ " " ++ serialize r1 ++ " " ++ serialize r2
  serialize (LDSA i)     = "ldsa" ++ " " ++ show i
  serialize (LDLA i)     = "ldla" ++ " " ++ show i
  serialize (LDAA i)     = "ldaa" ++ " " ++ show i
  serialize (STS i)      = "sts" ++ " " ++ show i
  serialize (STL i)      = "stl" ++ " " ++ show i
  serialize (STA i)      = "sta" ++ " " ++ show i
  serialize (STR r)      = "str" ++ " " ++ serialize r
  serialize (STMH i)     = "stmh" ++ " " ++ show i
  serialize (AJS i)      = "ajs" ++ " " ++ show i
  serialize (LINK i)     = "link" ++ " " ++ show i
  serialize (UNLINK)     = "unlink"
  serialize (ADD)        = "add"
  serialize (SUB)        = "sub"
  serialize (MUL)        = "mul"
  serialize (DIV)        = "div"
  serialize (MOD)        = "mod"
  serialize (NEG)        = "neg"
  serialize (AND)        = "and"
  serialize (OR)         = "or"
  serialize (XOR)        = "xor"
  serialize (NOT)        = "not"
  serialize (EQ)         = "eq"
  serialize (NE)         = "ne"
  serialize (LT)         = "lt"
  serialize (GT)         = "gt"
  serialize (LE)         = "le"
  serialize (GE)         = "ge"
  serialize (BEQ s)      = "beq" ++ " " ++  s
  serialize (BNE s)      = "bne" ++ " " ++ s
  serialize (BLT s)      = "blt" ++ " " ++ s
  serialize (BGT s)      = "bgt" ++ " " ++ s
  serialize (BLE s)      = "ble" ++ " " ++ s
  serialize (BGE s)      = "bge" ++ " " ++ s
  serialize (BRA s)      = "bra" ++ " " ++  s
  serialize (BRF s)      = "brf" ++ " " ++  s
  serialize (BRT s)      = "brt" ++ " " ++  s
  serialize (BSR s)      = "bsr" ++ " " ++  s
  serialize (JSR)        = "jsr"
  serialize (RET)        = "ret"
  serialize (TRAP i)     = "trap" ++ " " ++ show i
