import           CodeGen.CodeGenerator
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.List
import           Parsing.Lexer
import           Parsing.ParserBase        hiding ((<*>))
import           Parsing.ParserImpl
import           Parsing.PrettyPrinter
import           Shared.AST
import           Shared.Result
import           Shared.SymbolTable
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Typing.SemanticAnalyzer
import           Typing.TypeChecker

-- With MaybeT we can combine IO monad with Maybe monad.
nothing :: MaybeT IO a
nothing = MaybeT (pure Nothing)

just :: a -> MaybeT IO a
just a = MaybeT (pure (Just a))

readSourceFile :: String -> MaybeT IO String
readSourceFile filename =  do
        handle <- liftIO $ openFile filename ReadMode
        liftIO $ hGetContents handle

doLexing :: String -> MaybeT IO  [Result Token]
doLexing contents
        | containsError tokens = (liftIO $ putStrLn (viewErrors contents tokens)) >> nothing
        | otherwise            = just tokens
        where tokens = lexer contents

doParsing :: [Result Token] -> String -> MaybeT IO (Result SPL)
doParsing tokens contents
        | isError ast = (liftIO $ putStrLn (viewError contents ast)) >> nothing
        | otherwise   = just ast
        where ast = parser tokens

doSymbolTable :: Result SPL -> MaybeT IO (Result SymbolTable)
doSymbolTable ast =
        do
                let symbols = fmap buildSymbolTable ast
                just symbols

doTypeInference :: Result SPL -> Result SymbolTable -> String -> MaybeT IO (Result SymbolTable)
doTypeInference ast symbolTable contents
        | isError symbols = (liftIO $ putStrLn (viewError contents symbols)) >> nothing
        | otherwise =  just symbols
        where
                symbols = do
                        spl <- ast
                        symbolTable <- symbolTable
                        typeInference spl symbolTable

doSemanticAnalysis :: Result SPL -> MaybeT IO (Result SPL)
doSemanticAnalysis ast = just f
        where f = do
                spl <- ast
                spl <- checkProgram spl
                pure spl

doPrettyPrint :: Result SPL -> MaybeT IO String
doPrettyPrint ast = just (prettyPrint ast)

doCodeGen :: Result SPL -> Result SymbolTable -> String -> MaybeT IO String
doCodeGen (Error _ _) _ _                    = nothing
doCodeGen _ (Error _ _) _                    = nothing
doCodeGen (Ok ast _) (Ok symbols _) filename = just $ codeGen symbols ast

compile filename = (runMaybeT $ do
        liftIO $ putStrLn filename

        contents <-  readSourceFile filename
        tokens <- doLexing contents
        ast <- doParsing tokens contents
        prettyPrint <- doPrettyPrint ast
        ast <- doSemanticAnalysis ast
        symbolTable <- doSymbolTable ast
        symbolTable <- doTypeInference ast symbolTable contents
        il <- doCodeGen ast symbolTable filename

        liftIO $ createDirectoryIfMissing False "./out"
        liftIO $ writeFile ("./out/" ++ (takeBaseName filename) ++ ".ssm") il
        liftIO $ writeFile ("./out/" ++ (takeBaseName filename) ++ ".pretty")  prettyPrint
        ) >> pure ()

main = do
        args <- getArgs
        case args of
          [file] -> do
                compile file
          _ -> putStrLn "Wrong number of arguments."
