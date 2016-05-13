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
import Utils

-- Tree structure
data FNode a = FNode a [FNode a]
    deriving (Eq,Ord,Show)

type Feat = (String, String) -- Feature string and what scheme

clearDB :: Connection -> IO ()
clearDB conn = do
    execute_ conn "delete from hs_lemma_feature"
    execute_ conn "delete from hs_lemma_using"
    execute_ conn "delete from hs_lemma" 
    return ()


-- Inserts a list of lemmas with their induction variables
insertLemmas :: Name a => Connection -> [Formula a] -> Theory a -> IO ()
insertLemmas conn [] _ = return ()
insertLemmas conn (f:xs) thy = do
    let indVariables = getInductionVariables $ fm_info f
    --let finalIndVars = if (length indVariables > 0) then [0] else []
    let finalIndVars = indVariables
    execute conn "insert into hs_lemma (name, indvars, body) values (?, ?, ?) " [(fromJust $ getFmName f), ("{" ++ (intercalate "," (map show finalIndVars)) ++ "}"), showFormula f thy ]
    insertLemmas conn xs thy

-- Inserts a list of features
insertFeatures :: Connection -> [(String, [Feat])] -> IO ()
insertFeatures conn [] = return ()
insertFeatures conn ((lemma, features):xs) = do
    let (flist, slist) = unzip features
    let values = zip3 (take (length features) (repeat lemma)) flist slist
    executeMany conn "insert into hs_lemma_feature (lemma, feature, scheme) values (?,?,?)" values
    insertFeatures conn xs

-- Inserts a list of function features
insertFunctionFeatures :: Connection -> [(String, [Feat])] -> IO ()
insertFunctionFeatures conn [] = return ()
insertFunctionFeatures conn ((lemma, features):xs) = do
    let (flist, slist) = unzip features
    let values = zip3 (take (length features) (repeat lemma)) flist slist
    executeMany conn "insert into hs_function_feature (function, feature, scheme) values (?,?,?)" values
    insertFunctionFeatures conn xs

removeDuplicates :: [(String, [Feat])] -> [(String, [Feat])]
removeDuplicates ((lemma, feats):xs) = (lemma, nub feats):(removeDuplicates xs)
removeDuplicates [] = []

getInductionVariables :: Name a => Info a -> [Int]
getInductionVariables (Lemma _ _ (Just ps)) = indVars ps

-- Printing a tree using indentation
printTree :: String -> FNode String -> IO ()
printTree sep (FNode id fs) = do putStrLn $ sep ++ id; mapM_ (printTree (sep ++ "  ")) fs

-- Extract subtrees with certain depth. Returns all possible trees as a list
extractSubTrees :: Int -> FNode String -> [FNode String]
extractSubTrees 0 _ = []
--extractSubTrees depth f@(FNode "==" fs) = (concat $ map (extractSubTrees depth) fs) -- Skipping the equality symbol
--extractSubTrees depth f@(FNode "Equals" fs) = (concat $ map (extractSubTrees depth) fs) -- Skipping the equality symbol
--extractSubTrees depth f@(FNode "match" fs) = (concat $ map (extractSubTrees depth) fs) -- Skipping the match symbol
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
mergeStrings x1 x2 = x2 ++ "," ++ x1


emptyLemmaList :: [String] -> [(String, [Feat])]
emptyLemmaList ls = zip ls (take (length ls) (repeat []))

generateHalf :: [[(String, [Feat])]] -> [(String, [Feat])]
generateHalf [] = []
generateHalf ls = foldl preMerge (head ls) (tail ls)

printList :: [(String, [Feat])] -> IO ()
printList ((lemma, []):xss) = do
    putStrLn ""
    printList xss
printList ((lemma, ((fString, fScheme):fs)):xss) = do
    putStrLn $ lemma ++ " = " ++ fString ++ ", " ++ fScheme
    printList ((lemma, fs):xss)
printList [] = return ()

preMerge :: [(String, [Feat])] -> [(String, [Feat])] -> [(String, [Feat])]
preMerge xs ys = preMerge' (sort xs) (sort ys) -- We can assume the lists are equally long here

preMerge' :: [(String, [Feat])] -> [(String, [Feat])] -> [(String, [Feat])]
preMerge' xs ys = map (\((n, f1),(_, f2)) -> (n, nub $ f1 ++ f2)) $ zip xs ys

-- Not sure about the complexity of this one hehe :-)
mergeFeatures :: Bool -> [(String, [Feat])] -> [(String, [Feat])] -> [(String, [Feat])]
mergeFeatures sslf ((n, feats):ls) fs = (n, extendedFeats):(mergeFeatures sslf ls fs)
    where
        extendedFeats = (filter (\y -> (not sslf && not (isSymbolicLemmaFeat y)) || sslf) feats) ++ (nub addedFeats)
        addedFeats = concat $ map (\(n', f) -> f) funcsOfLemma
        funcsOfLemma = filter (\(functionName, _) -> any (\(feature, scheme) -> scheme == "ls" && feature == functionName) feats) fs -- finding the function features which has its key anywhere in the features of the lemma
mergeFeatures _ [] _ = []

isSymbolicLemmaFeat :: Feat -> Bool
isSymbolicLemmaFeat (_, "ls") = True
isSymbolicLemmaFeat _ = False
