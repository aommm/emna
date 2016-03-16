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
import Data.List
import Debug.Trace

-- Datatype for listing the features of a formula
data Features a = Features
    { features :: [a] 
    } deriving (Eq,Ord,Show)

data Feature a = SingleFeature a -- One lonely symbol
    | MultiFeature a [Feature a] -- Symbol with inner symbols
    | EmptyFeature -- Not defined symbol
    deriving (Eq,Ord,Show)

emptyFeatures :: Features (Feature Id)
emptyFeatures = Features []

mergeFeatures :: Features (Feature Id) -> Features (Feature Id) -> Features (Feature Id)
mergeFeatures (Features fs1) (Features []) = Features fs1
mergeFeatures f1@(Features fs1) (Features (f:fs2)) = mergeFeatures (addFeature f1 f) (Features fs2)

getFeature :: Features (Feature Id) -> Maybe (Feature Id)
getFeature (Features []) = Nothing
getFeature (Features (x:xs)) = Just x

addFeature :: Features (Feature Id) -> Feature Id -> Features (Feature Id)
addFeature (Features fs) f
    | any (equalsFeature f) fs = Features fs
    | otherwise = Features $ (fs++[f])

addFeatures :: Features (Feature Id) -> [Feature Id] -> Features (Feature Id)
addFeatures f' [] = f'
addFeatures f' (f:fs) = addFeature (addFeatures f' fs) f

equalsFeature :: Feature Id -> Feature Id -> Bool
equalsFeature (SingleFeature a) (SingleFeature b) = (idUnique a) == (idUnique b)
equalsFeature _ _ = False

printFeatures :: Features (Feature Id) -> IO ()
printFeatures (Features []) = return ()
printFeatures (Features (f:fs)) = do
    putStrLn $ featureToString f
    printFeatures (Features fs)

featureToString :: Feature Id -> String
featureToString EmptyFeature = "_"
featureToString (SingleFeature f) = idString f
featureToString (MultiFeature f []) = idString f
featureToString (MultiFeature f fs) = idString f ++ "(" ++ (featuresToString fs " ") ++ ")"

featuresToString :: [Feature Id] -> String -> String
featuresToString [] _ = ""
featuresToString [f] sep = (featureToString f)
featuresToString (f:fs) sep = (featureToString f) ++ sep ++ (featuresToString fs sep)

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
    -- readFeature xs

extractFromExpressions :: [Expr Id] -> Features (Feature Id) -> IO (Features (Feature Id))
extractFromExpressions [] fs = do return fs
extractFromExpressions (x:xs) fs = do
    fs1 <- extractFromExpr x fs
    fs2 <- extractFromExpressions xs fs1 
    return fs2

extractFromExpr :: Expr Id -> Features (Feature Id) -> IO (Features (Feature Id))
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

        -- Use of global function
        -- This one is tricky
        -- For example, xs ++ ys we want to generate the following 4 features:
        -- `++`, `list ++`, `++ list` and `list ++ list`.
        -- Currently 
        Gbl (Global name typ args) :@: exps -> do
            f1 <- extractFromExpressions exps fs
            return $ addFeature f1 (MultiFeature name (take (length exps) (repeat EmptyFeature)))

        -- Local variable, we are looking for the type
        Lcl (Local name (TyCon feature _)) -> do
            return $ addFeature fs (SingleFeature feature)

        _ -> do
            -- putStrLn $ show expr
            return fs

-- First step in generating the feature combos
createMultiFeatures :: Features (Feature Id) -> [Feature Id] -> Id -> Int -> Features (Feature Id)
createMultiFeatures f' [] name d = addFeature f' (MultiFeature name (take d (repeat EmptyFeature))) -- The empty case
createMultiFeatures f' (f:fs) name d = addFeatures f' (generateCombos f fs name)

generateCombos :: Feature Id -> [Feature Id] -> Id -> [Feature Id]
generateCombos base [] name = [MultiFeature name [base], MultiFeature name [EmptyFeature]]
generateCombos base (a:as) name = (extendCombos emptyCase (generateCombos a as name)) ++ (extendCombos baseCase (generateCombos a as name))
    where 
        baseCase = MultiFeature name [base]
        emptyCase = MultiFeature name [EmptyFeature]

extendCombos :: Feature Id -> [Feature Id] -> [Feature Id]
extendCombos b [] = []
extendCombos b@(MultiFeature name [base]) ((MultiFeature _ f'):fs) = (MultiFeature name (base:f')):(extendCombos b fs)