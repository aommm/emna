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
    putStrLn "\n#######################################"
    putStrLn $ "Looking at lemma " ++ (fromJust $ getFmName f)
    putStrLn $ "--------------------------------------"
    pprint f
    putStrLn $ "--------------------------------------"
    let trees = extractSubTrees 2 (buildTree (fm_body f))
    
    -- readLibrary xs

-- Printing a tree using indentation
printTree :: String -> FNode Id -> IO ()
printTree sep (FNode id fs) = do putStrLn $ sep ++ idString id; mapM_ (printTree (sep ++ "  ")) fs
    
-- Builds a tree for an expression, recursively :)
buildTree :: Expr Id -> FNode Id
buildTree (Quant _ _ _ e') = buildTree e'
buildTree (Builtin Equal :@: exps) = FNode (Id "==" 0 (Just (0,0))) $ map buildTree exps
buildTree (Lcl (Local name (TyCon feature _))) = FNode feature []
buildTree (Gbl (Global name typ args) :@: exps) = FNode name $ map buildTree exps
buildTree _ = FNode (Id "unknown" 0 (Just (0,0))) []

-- Extract subtrees with certain depth. Returns all possible trees as a list
extractSubTrees :: Int -> FNode Id -> [FNode Id]
extractSubTrees 1 (FNode id fs) = [(FNode id [])]
extractSubTrees depth (FNode id fs) = [(FNode id (concat $ map (extractSubTrees (depth-1)) fs))] ++ (concat $ map (extractSubTrees depth) fs)