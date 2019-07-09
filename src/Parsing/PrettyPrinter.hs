module Parsing.PrettyPrinter where

import           Shared.AST
import           Shared.Result

prettyPrint :: PrettyPrint a => a -> String
prettyPrint = write 0

class PrettyPrint a where
  write :: Int -> a -> String

instance PrettyPrint a => PrettyPrint (Result a) where
  write n (Ok a cts)      = write n a
  write n (Error msg ctx) = msg

instance PrettyPrint SPL where
  write n (SPL classes vars funcs) = concat (fmap (write n) classes) ++ concat (fmap (write n) vars) ++ concat (fmap (write n) funcs)

instance PrettyPrint ClassDecl where
  write n (ClassDecl i as _) = indent n ++ "class " ++ write 0 i ++  " {\n" ++ writeList 1 "\n" as ++ "\n}\n\n"

instance PrettyPrint Attribute where
  write n (Attribute i t _) = indent n ++  write 0 i ++  " :: " ++  write 0 t ++ "; "

instance PrettyPrint VarDecl where
  write n (VarDecl t i e _) = indent n ++ write 0 t ++ " " ++ write 0 i ++ " = " ++ write 0 e ++ ";\n"

instance PrettyPrint FunDecl where
  write n (FunDecl name par t decl stmt _) = indent n
    ++ write 0 name ++ " (" ++ writeList 0 ", " par ++ ") :: " ++ write 0 t ++ " {\n"
    ++ (writeList (n+1) "" decl)
    ++ (writeList (n+1) "" stmt) ++ "}\n\n"

instance PrettyPrint Type where
  write n t = show t

instance PrettyPrint Stmt where
  write n (If e s1 s2) = indent n
    ++ "if (" ++ write 0 e ++ "){\n" ++ writeList (n+1) "" s1 ++ indent n ++ "}\n"
      ++ case s2 of
        Nothing -> ""
        Just s -> indent n ++ "else {\n" ++ writeList (n+1) "" s ++ indent n ++ "}\n"
  write n (While e s) = indent n
    ++ "while (" ++ write 0 e ++ "){\n" ++ writeList (n+1) ""s ++ indent n ++"}\n"
  write n (Assign id e _) = indent n
    ++ write n id ++  " = " ++ write 0 e ++ ";\n"
  write n (FunCallStmt id e _) = indent n
    ++ write n id ++ " (" ++ writeList 0 ", " e ++ ");\n"
  write n (Return e _) = indent n
    ++ "return" ++ (case e of
      Nothing -> ""
      Just e1 -> " " ++ write 0 e1) ++ ";\n"


instance PrettyPrint Exp where
  write n (VarExp i _) = write 0 i
  write n (UnExp op e _) = write 0 op ++ write 0 e
  write n (IntExp i _) = show i
  write n (CharExp c _) = show c
  write n (BoolExp b _) = show b
  write n (FunCallExp i e _) = write 0 i ++ "(" ++ writeList 0 ", " e ++ ")"
  write n (TupleExp e1 e2 _) = "(" ++ write 0 e1 ++ ", " ++ write 0 e2 ++ ")"
  write n (EmptyList _) = "[]"
  write n (BinExp op e1 e2 _) = "(" ++ write 0 e1 ++ " " ++  write 0 op ++ " " ++  write 0 e2  ++ ")"

instance PrettyPrint UnaryOp where
  write n Neg = "!"
  write n Min = "-"

instance PrettyPrint BinaryOp where
  write n Add  = "+"
  write n Sub  = "-"
  write n Mult = "*"
  write n Div  = "/"
  write n Mod  = "%"
  write n Eq   = "=="
  write n Lt   = "<"
  write n Gt   = ">"
  write n LtEq = "<="
  write n GtEq = ">="
  write n NEq  = "!="
  write n And  = "&&"
  write n Or   = "||"
  write n Cons = ":"
  write n App  = "."

instance PrettyPrint Char where
  write n c = c:[]

instance PrettyPrint a => PrettyPrint [a]  where
  write n []     = ""
  write n (x:xs) = write n x ++ write n xs

join :: String -> [String] -> String
join seperator strs = concat (join' seperator strs )

join' :: String -> [String] -> [String]
join' seperator []     = [""]
join' seperator [x]    = [x]
join' seperator (x:xs) = (x:seperator:join' seperator xs)

indent :: Int -> String
indent 0 = ""
indent n = ("    "++indent (n-1))

writeList :: PrettyPrint a => Int -> String -> [a] -> String
writeList n seperator xs = join seperator (fmap (write n) xs)
