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
import Control.Monad

-- Tree structure
data FNode a = FNode a [FNode a]
    deriving (Eq,Ord,Show)

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
                Left msg      -> error $ "Parsing library failed:"++show msg
                Right (Library _ _ ls) -> do
                    putStrLn "Loaded library"
                    readLibrary (M.elems ls)

-- Going through each lemma of the library
readLibrary :: [Formula Id] -> IO ()
readLibrary [] = return ()
readLibrary (f:xs) = do
    putStrLn $ (fromJust $ getFmName f)
    let tree = buildTree (fm_body f)
    let trees = extractSubTrees 3 tree
    let features = nub $ concat $ map extractFeatures trees
    putStrLn $ show features
    readLibrary xs

-- Printing a tree using indentation
printTree :: String -> FNode Id -> IO ()
printTree sep (FNode id fs) = do putStrLn $ sep ++ idString id; mapM_ (printTree (sep ++ "  ")) fs

-- Builds a tree for an expression, recursively :)
buildTree :: Expr Id -> FNode Id
buildTree (Quant _ _ _ e') = buildTree e'
buildTree (Builtin Equal :@: exps) = FNode (Id "==" 0 (Just (0,0))) $ map buildTree exps
buildTree (Lcl (Local name (TyCon feature _))) = FNode feature []
buildTree (Lcl (Local name (TyVar feature))) = FNode (Id "anyType" 0 (Just (0,0))) []
buildTree (Gbl (Global name typ args) :@: exps) = FNode name $ map buildTree exps
buildTree _ = FNode (Id "unknown" 0 (Just (0,0))) []

-- Extract subtrees with certain depth. Returns all possible trees as a list
extractSubTrees :: Int -> FNode Id -> [FNode Id]
extractSubTrees depth f@(FNode id fs) = [thisSubTree] ++ otherSubTrees
    where
        thisSubTree =  extractSubTree depth f --(FNode id (concat $ map (extractSubTrees (depth-1)) fs))
        otherSubTrees = (concat $ map (extractSubTrees depth) fs)

-- Extract one subtree
extractSubTree :: Int -> FNode Id -> FNode Id
extractSubTree 1 (FNode id _) = (FNode id [])
extractSubTree depth (FNode id fs) = (FNode id (map (extractSubTree (depth-1)) fs))

-- Extracts features from a tree
extractFeatures :: FNode Id -> [String]
extractFeatures (FNode id []) = [(idString id)]
extractFeatures (FNode id f') = [(idString id)] ++ (map (\x -> (idString id) ++ "(" ++ x ++ ")") combos)
    where
        arguments = map extractFeatures f'
        combos = combineArguments arguments

-- Combines a list of arguments
combineArguments :: [[String]] -> [String]
combineArguments [xs] = xs
combineArguments (xs:xss) = concat $ map (combine' xs) (combineArguments xss)

-- Two helper functions for combineArguments
combine' :: [String] -> String -> [String]
combine' xs1 x = (map (mergeStrings x) xs1) ++ [x] ++ xs1

mergeStrings :: String -> String -> String
mergeStrings x1 x2 = x2 ++ ", " ++ x1