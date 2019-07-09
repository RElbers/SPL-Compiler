{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Parsing.ParserImpl where

import           Debug.Trace
import           Parsing.Lexer
import           Parsing.ParserBase
import           Prelude            hiding (return, (*>), (<$), (<$>), (<*),
                                     (<*>), (<:>), (<|>), (>>=))
import           Shared.AST
import           Shared.Result

parser tks = case (parse :: Parser Token SPL) (unwrap tks) of (a, b) -> a
    where
        unwrap :: [Result Token] -> [(Token, Context)]
        unwrap []                  = []
        unwrap ((Ok tkn ctx):rest) = ((tkn, ctx):unwrap rest)
        unwrap _                   = []

----------------------
-- Parser Instances --
----------------------
instance Parsable SPL where
    parse = funkyApply <$>( SPL <$> (|*)parse ) <*> splitDecls <* endOfFile
        where
            funkyApply f (a,b) = f a b

            parseDecls :: Parser Token (Either VarDecl FunDecl)
            parseDecls = Left <$> parse
                <|> Right <$> parse

            splitDecls :: Parser Token ([VarDecl], [FunDecl])
            splitDecls =
                let
                    vars []              = []
                    vars (Left v:decls)  = v:vars decls
                    vars (Right f:decls) = vars decls
                    funcs []              = []
                    funcs (Left v:decls)  = funcs decls
                    funcs (Right f:decls) = f:funcs decls
                    splitter :: [Either VarDecl FunDecl] -> ([VarDecl], [FunDecl])
                    splitter decls = (vars decls, funcs decls)
                in
                    splitter <$> (|+) parseDecls

instance Parsable ClassDecl where
    parse = ClassDecl <$ reserved "class" <*>  parse <* reserved "{" <*>  parse <* reserved "}" <! parse

instance Parsable Attribute where
    parse = Attribute <$> parse <* reserved "::" <*> parse  <! parse

instance Parsable VarDecl where
    parse = VarDecl <$> typeParser <*> parse <* reserved "=" <*> parse <* reserved ";" <! parse
        where
            typeParser :: Parser Token Type
            typeParser =
                Untyped <$ reserved "var"
                <|> parse

instance Parsable FunDecl where
    parse = FunDecl <$>  parse <*> parens (parse) <* reserved "::" <*> parseFunType <* reserved "{" <*> (|*) parse <*> (|+) parse <* reserved "}" <! parse
        where parseFunType = TFunc <$> (|*) parse <* reserved "->" <*> parse

instance Parsable [Attribute] where
    parse = (:) <$> parse <* reserved ";" <*> parse
        <|> (:) <$> parse <*> return []
        <|> return []

instance Parsable [String] where
    parse = (:) <$> parse <* reserved "," <*> parse
        <|> (:) <$> parse <*> return []
        <|> return []

instance Parsable [Exp] where
    parse = (:) <$> parse <* reserved "," <*> parse
        <|> (:) <$> parse <*> return []
        <|> return []

instance Parsable Type where
    parse = parens (TTuple <$> parse <* reserved "," <*>  parse)
        <|> brackets (TList <$> parse)
        <|> TBool <$ reserved "Bool"
        <|> TInt <$ reserved "Int"
        <|> TChar <$ reserved "Char"
        <|> TVoid <$ reserved "Void"
        <|> TVar <$> parse

instance Parsable Stmt where
    parse = parens(parse)
        <|> If <$ reserved "if" <*> parens (parse) <*> curly ((|*) parse) <*> (|?) (reserved "else" *> curly ((|*) parse) )
        <|> While <$ reserved "while" <*> parens (parse) <*> curly ((|*) parse)
        <|> FunCallStmt <$> parse <*> parens(parse) <* reserved ";" <! parse
        <|> Return <$ reserved "return" <*> (|?) parse <* reserved ";"  <! parse
        <|> fieldsToSetters <$> ((,,) <$> parse <*> (|*) parse <* reserved "=" <*> parse  <* reserved ";" ) <! parse

instance Parsable Exp where
    parse = parseR
        where
            parseL = foldr pChainL (parse') parseBinOpL
            parseR = foldr pChainR (parseL) parseBinOpR
            parse' =  parens (parse)
                <|> parens (TupleExp  <$> parse <* reserved "," <*> parse) <! parse
                <|> quotes (CharExp <$> parse) <! parse
                <|> FunCallExp <$> parse <*> parens(parse) <! parse
                <|> UnExp <$> parse <*> parse <! parse
                <|> VarExp <$> parse <! parse
                <|> IntExp <$> parse <! parse
                <|> BoolExp <$> parse <! parse
                <|> EmptyList <$ reserved "[" <* reserved "]" <! parse

            parseOps ops = choice [op <$ reserved c <! parse | (c, op) <- ops]
            parseBinOpApp = map parseOps ([[("==", hack $ BinExp Sub)]])
            parseBinOpL = map parseOps opPrecedenceLeft
            parseBinOpR = map parseOps opPrecedenceRight

instance Parsable Field where
    parse =  Field <$ reserved "." <*> parse <! parse

instance Parsable UnaryOp where
    parse = Neg <$  reserved "!"
        <|> Min <$  reserved "-"

instance Parsable BinaryOp where
    parse = Add <$  reserved "+"
        <|> Sub <$ reserved "-"
        <|> Mult <$ reserved "*"
        <|> Div <$ reserved "/"
        <|> Mod <$ reserved "%"
        <|> Eq <$ reserved "=="
        <|> LtEq <$ reserved "<="
        <|> GtEq <$ reserved ">="
        <|> Lt <$ reserved "<"
        <|> Gt <$ reserved ">"
        <|> NEq <$ reserved "!="
        <|> And <$ reserved "&&"
        <|> Or <$ reserved "||"
        <|> Cons <$ reserved ":"


fieldsToSetters :: (String, [Field], Exp)  -> Context -> Stmt
fieldsToSetters (id, [], exp) ctx     = Assign id exp ctx
fieldsToSetters (id, fields, exp) ctx = Assign id (makeExp id fields exp) ctx
    where
        makeExp :: String -> [Field] -> Exp -> Exp
        makeExp id fields exp = makeExp' id fields [] exp
            where
                makeExp' id [] ffs exp' = exp'
                makeExp' id (f@(Field name ctx):fs) ffs exp' = FunCallExp (setter f) [(fieldsToGetters (id, reverse ffs) ctx), makeExp' id fs (f:ffs) exp'] ctx



fieldsToGetters :: (String, [Field]) -> Context -> Exp
fieldsToGetters (id, fields) = fieldsToGetters' id (reverse fields)
    where
        fieldsToGetters' id [] = VarExp id
        fieldsToGetters' id (f@(Field name ctx):fs) = FunCallExp (getter f) [(fieldsToGetters' id fs ctx)]
