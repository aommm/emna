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

-- Inserts a list of lemmas with their induction variables
insertLemmas :: Name a => Connection -> [Formula a] -> IO ()
insertLemmas conn [] = return ()
insertLemmas conn (f:xs) = do
    execute conn "insert into hs_lemma (name, indvars) values (?, ?) " [(fromJust $ getFmName f), ("{" ++ (intercalate "," (map show (getInductionVariables $ fm_info f))) ++ "}")]
    insertLemmas conn xs

-- Inserts a list of features
insertFeatures :: Connection -> [(String, [String])] -> IO ()
insertFeatures conn [] = return ()
insertFeatures conn ((lemma, features):xs) = do
    let values = zip (take (length features) (repeat lemma)) features
    executeMany conn "insert into hs_lemma_feature (lemma, feature) values (?,?)" values
    insertFeatures conn xs

getInductionVariables :: Name a => Info a -> [Int]
getInductionVariables (Lemma _ _ (Just p)) = indVars p

-- Printing a tree using indentation
printTree :: String -> FNode String -> IO ()
printTree sep (FNode id fs) = do putStrLn $ sep ++ id; mapM_ (printTree (sep ++ "  ")) fs

-- Extract subtrees with certain depth. Returns all possible trees as a list
extractSubTrees :: Int -> FNode String -> [FNode String]
extractSubTrees depth f@(FNode id fs) = [thisSubTree] ++ otherSubTrees
    where
        thisSubTree = extractSubTree depth f --(FNode id (concat $ map (extractSubTrees (depth-1)) fs))
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
-- input: [[rev],[list]]
combineArguments :: [[String]] -> [String]
combineArguments [xs] = xs
combineArguments (xs:xss) = concat $ map (combine' xs) (combineArguments xss)

-- Two helper functions for combineArguments
-- [rev] -> list
-- ["rev, list"] ++ ["list"] ++ ["rev"] = 
combine' :: [String] -> String -> [String]
combine' xs1 x = (map (mergeStrings x) xs1) ++ [x] ++ xs1

-- return "rev, list"
mergeStrings :: String -> String -> String
mergeStrings x1 x2 = x2 ++ ", " ++ x1
