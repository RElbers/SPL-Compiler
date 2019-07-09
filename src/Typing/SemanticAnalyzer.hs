module Typing.SemanticAnalyzer where

import           Data.List
import           Shared.AST
import           Shared.Result

checkProgram :: SPL -> Result SPL
checkProgram spl = do
    spl <- hasMain spl
    spl <- checkReturn spl
    spl <- ctorConflict spl
    pure spl

hasMain :: SPL -> Result SPL
hasMain (SPL _ _ []) = Error "Program does not contain a main function." eof
hasMain spl@(SPL c v (f:fs))
    | nameOf f  == "main" = pure spl
    | otherwise      = do
        spl'@(SPL c' v' fs') <- hasMain (SPL c v fs)
        pure (SPL c' v' (f:fs'))
    where
        nameOf (FunDecl name _ _ _ _ _) = name

ctorConflict :: SPL -> Result SPL
ctorConflict spl@(SPL classes _ fs) =
    let
        fname (FunDecl n _ _ _ _ _) = n
        ctors = fmap (\(ClassDecl n _ _) -> n) classes
        fnames = fmap (\f -> fname f) fs
        same = intersect ctors fnames

        badHombre = case same of
            []     -> Nothing
            (n:ns) -> find (\f -> n == fname f) fs
    in
        case badHombre of
            Nothing -> pure spl
            Just (FunDecl n _ _ _ _ ctx) -> Error ("The function '" ++ n ++ "' has the same name as a class." ) ctx

class CheckReturn a where
    checkReturn :: a -> Result a

instance CheckReturn a => CheckReturn [a] where
    checkReturn []     = pure []
    checkReturn (x:xs) = (:) <$> checkReturn x <*> checkReturn xs

instance CheckReturn SPL where
    checkReturn spl@(SPL _ _ [])     = pure spl
    checkReturn spl@(SPL c v (f:fs)) = do
        f' <- checkReturn f
        fs' <- checkReturn fs
        return (SPL c v (f':fs'))

instance CheckReturn FunDecl where
    checkReturn (FunDecl s a t@(TFunc _ TVoid) l [] c) = pure (FunDecl s a t l [Return Nothing c] c)
    checkReturn f@(FunDecl s a t@(TFunc _ TVoid) l stmts c) = case last stmts of
        Return _ _ -> pure f
        otherwise  -> pure (FunDecl s a t l (stmts ++ [Return Nothing c]) c)

    checkReturn (FunDecl s a t l [] c) = Error "Function has no return statement." c
    checkReturn f@(FunDecl s a t l stmts c) = (\stmts' -> (FunDecl s a t l stmts' c)) <$> checkReturnLast stmts c

instance CheckReturn Stmt where
    checkReturn (Assign _ _ c) =  Error "Functions must end with a return statement." c
    checkReturn (FunCallStmt _ _ c) =  Error "Functions must end with a return statement." c
    checkReturn r@(Return _ c) =  Ok r c

    checkReturn (While e stmts) = checkReturn (If e stmts Nothing)
    checkReturn (If e stmts Nothing)  = (\stmts' -> If e stmts' Nothing) <$> checkReturnLast stmts eof
    checkReturn (If e stmts1 (Just stmts2))  = (\stmts1' stmts2' -> If e stmts1' (Just stmts2')) <$> checkReturnLast stmts1 eof <*> checkReturnLast stmts2 eof


checkReturnLast :: CheckReturn a => [a] -> Context -> Result [a]
checkReturnLast [] c = Error "Function has no return statement." c
checkReturnLast (stmts) c = (\lst -> init stmts ++ [lst]) <$> checkReturn (last stmts)
