import System.Environment
import System.Directory
import Tip.Parser (parseLibrary)
import Tip.Parser.Convert
import Tip.Library
import Data.Map (Map)
import qualified Data.Map as M
import Tip.Types
import Tip.Pretty
import Data.Maybe

-- Datatype for listing the features of a formula
data Features a = Features
    { features :: [a] 
    } deriving (Eq,Ord,Show)

emptyFeatures :: Features Id
emptyFeatures = Features []

mergeFeatures :: Features Id -> Features Id -> Features Id
mergeFeatures (Features fs1) (Features []) = Features fs1
mergeFeatures f1@(Features fs1) (Features (f:fs2)) = mergeFeatures (addFeature f1 f) (Features fs2)

addFeature :: Features Id -> Id -> Features Id
addFeature (Features fs) f
    | any (hasFeature f) fs = Features fs
    | otherwise = Features $ f:fs

hasFeature :: Id -> Id -> Bool
hasFeature a b = (idUnique a) == (idUnique b)

printFeatures :: Features Id -> IO ()
printFeatures (Features []) = return ()
printFeatures (Features (f:fs)) = do
    putStrLn $ idString f
    printFeatures (Features fs)

-- Reads a library file to begin with
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    exists <- doesFileExist filePath
    case exists of
        False -> putStrLn "File not found"
        True -> do
            libraryString <- readFile filePath
            case parseLibrary libraryString of
                Left msg      -> error $ "parsing library failed:"++show msg
                Right library -> do
                    putStrLn "loaded library"
                    saveLibrary library

saveLibrary :: Library Id -> IO ()
saveLibrary (Library _ _ ls) = do 
    readFeature $ M.elems ls

readFeature :: [Formula Id] -> IO ()
readFeature [] = return ()
readFeature (f:xs) = do
    let fs = emptyFeatures
    putStrLn "\n#######################################"
    putStrLn $ "Looking at lemma " ++ (fromJust $ getFmName f)
    putStrLn $ "--------------------------------------"
    pprint f
    putStrLn $ "--------------------------------------"

    fs <- extractFromExpr (fm_body f) fs
    printFeatures fs
    readFeature xs

extractFromExpressions :: [Expr Id] -> Features Id -> IO (Features Id)
extractFromExpressions [] fs = do return fs
extractFromExpressions (x:xs) fs = do
    fs1 <- extractFromExpr x fs
    return fs1

extractFromExpr :: Expr Id -> Features Id -> IO (Features Id)
extractFromExpr expr fs = do
    case expr of

        -- Top level expression, with the quantifier. We just go down looking for more expressions
        Quant _ _ _ expr -> do
            fs1 <- extractFromExpr expr fs
            return fs1

        -- Equality between two expressions. We extract features from both :)
        Builtin Equal :@: [exp1, exp2] -> do
            fs1 <- extractFromExpr exp1 fs
            fs2 <- extractFromExpr exp2 fs
            return $ mergeFeatures fs1 fs2

        -- Use of global function?
        Gbl (Global name typ args) :@: exps -> do
            fs1 <- extractFromExpressions exps fs
            return $ addFeature fs1 name

        -- Local variable, we are looking for the type
        Lcl (Local name (TyCon feature _)) -> do
            return $ addFeature fs feature

        _ -> do
            -- putStrLn $ show expr
            return fs