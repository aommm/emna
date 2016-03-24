{-# LANGUAGE OverloadedStrings #-}

module FeatureExtraction where

import System.Environment
import System.Directory
import Tip.Parser (parseLibrary)
import Tip.Parser.Convert
import Tip.Library
import Data.Map (Map)
import qualified Data.Map as M
import Tip.Types
import Tip.Pretty
import Tip.Fresh
import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad
import Database.PostgreSQL.Simple
import Data.ByteString.Internal
import Data.ByteString.Char8 (pack)



-- Tree structure
data FNode a = FNode a [FNode a]
    deriving (Eq,Ord,Show)

clearDB :: Connection -> IO ()
clearDB conn = do
    execute_ conn "delete from hs_lemma_feature"
    execute_ conn "delete from hs_lemma_using"
    execute_ conn "delete from hs_lemma" 
    return ()

-- Inserts a all the lemmas
insertLemmas :: Connection -> [(String, [Int], [String])] -> IO ()
insertLemmas conn [] = return ()
insertLemmas conn ((lemma, vars, _):xs) = do
    execute conn "insert into hs_lemma (name, indvars) values (?, ?) " [lemma, ("{" ++ (intercalate "," (map show vars)) ++ "}")]
    insertLemmas conn xs

-- Inserts all the features
insertFeatures :: Connection -> [(String, [Int], [String])] -> IO ()
insertFeatures conn [] = return ()
insertFeatures conn ((lemma, _, features):xs) = do
    let values = zip (take (length features) (repeat lemma)) features
    executeMany conn "insert into hs_lemma_feature (lemma, feature) values (?,?)" values
    insertFeatures conn xs

-- Going through each lemma of the library
formulasToFeatures :: Name a => [Formula a] -> IO ([(String, [Int], [String])])
formulasToFeatures [] = return []
formulasToFeatures (f:xs) = do
    let tree = buildTree (fm_body f)
    let trees = extractSubTrees 3 tree
    let features = nub $ concat $ map extractFeatures trees
    rest <- formulasToFeatures xs
    return $ ((fromJust $ getFmName f), (getInductionVariables $ fm_info f), features):rest

getInductionVariables :: Name a => Info a -> [Int]
getInductionVariables (Lemma _ _ (Just (_, vars))) = vars

-- Printing a tree using indentation
printTree :: String -> FNode String -> IO ()
printTree sep (FNode id fs) = do putStrLn $ sep ++ id; mapM_ (printTree (sep ++ "  ")) fs

-- Builds a tree for an expression, recursively :)
buildTree :: Name a => Expr a -> FNode String
buildTree (Quant _ _ _ e') = buildTree e'
buildTree (Builtin Equal :@: exps) = FNode "==" $ map buildTree exps
buildTree (Lcl (Local name (TyCon feature _))) = FNode (varStr feature) []
buildTree (Lcl (Local name (TyVar feature))) = FNode "anyType" []
buildTree (Gbl (Global name typ args) :@: exps) = FNode (varStr name) $ map buildTree exps
buildTree _ = FNode "unknown" []

-- Extract subtrees with certain depth. Returns all possible trees as a list
extractSubTrees :: Int -> FNode String -> [FNode String]
extractSubTrees depth f@(FNode id fs) = [thisSubTree] ++ otherSubTrees
    where
        thisSubTree =  extractSubTree depth f --(FNode id (concat $ map (extractSubTrees (depth-1)) fs))
        otherSubTrees = (concat $ map (extractSubTrees depth) fs)

-- Extract one subtree
extractSubTree :: Int -> FNode String -> FNode String
extractSubTree 1 (FNode id _) = (FNode id [])
extractSubTree depth (FNode id fs) = (FNode id (map (extractSubTree (depth-1)) fs))

-- Extracts features from a tree
extractFeatures :: FNode String -> [String]
extractFeatures (FNode id []) = [id]
extractFeatures (FNode id f') = [id] ++ (map (\x -> (id) ++ "(" ++ x ++ ")") combos)
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