{-# LANGUAGE OverloadedStrings #-}

module AnalyticalFeatureExtraction where

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
import Text.Printf

import FeatureExtraction
import qualified SymbolicFeatureExtraction as SF

-- Quick analysis of a feature set
analyseSymbolic :: [(String, [String])] -> IO ([(String, [String])])
analyseSymbolic [] = return []
analyseSymbolic ((lemmaName, features):xs) = do
    let nFeats = length features -- number of features
    let nDistFeats = length $ nub features -- number of distinct features
    let ratio = intdiv nDistFeats nFeats -- 
    let mostPopular = most (freq features) -- most popular feature of the lemma

    let f' = ["_length " ++ (show nFeats), "_lengthDistinct " ++ (show nDistFeats), "_distinctRatio " ++ (printf "%.1f" $ ratio), "_popular " ++ mostPopular]

    rest <- analyseSymbolic xs
    return $ (lemmaName, f'):rest

analyseSymbolicLemmaFeatures :: (Show a, Name a) => [(String, [String])] -> Map String (Formula a) -> IO ([(String, [String])])
analyseSymbolicLemmaFeatures [] _ = return []
analyseSymbolicLemmaFeatures ((lemmaName, features):xs) ls = do
    fs <- analyseSymbolic [(lemmaName, features)]
    let (_, features') = head fs

    let same = sameSymbols (fm_body $ fromJust $ M.lookup lemmaName ls)
    let exactSame = exactSameSymbols (fm_body $ fromJust $ M.lookup lemmaName ls)
    let commutative = (if exactSame then (isCommutative (fm_body $ fromJust $ M.lookup lemmaName ls)) else False)
    let associative = (if exactSame then (isAssociative (fm_body $ fromJust $ M.lookup lemmaName ls)) else False)

    rest <- analyseSymbolicLemmaFeatures xs ls
    return $ (lemmaName, ["_commutative " ++ (show commutative), "_associative " ++ (show associative)]):rest

-- Extracting features from each side of the equality sign and then comparing the length of the difference
exactSameSymbols :: (Show a, Name a) => Expr a -> Bool
exactSameSymbols (Quant _ _ _ (Builtin Equal :@: [e1, e2])) = length (lhs \\ rhs) == 0
    where
        lhs = concat $ map extractFeatures $ extractSubTrees 1 $ SF.buildTree e1
        rhs = concat $ map extractFeatures $ extractSubTrees 1 $ SF.buildTree e2
exactSameSymbols _ = False

-- Extracting features from each side of the equality sign and then comparing the length of the difference.
-- Removes duplicates so we only look at the "same symbols but with different frequency"
sameSymbols :: (Show a, Name a) => Expr a -> Bool
sameSymbols (Quant _ _ _ (Builtin Equal :@: [e1, e2])) = length (lhs \\ rhs) == 0
    where
        lhs = nub $ concat $ map extractFeatures $ extractSubTrees 1 $ SF.buildTree e1
        rhs = nub $ concat $ map extractFeatures $ extractSubTrees 1 $ SF.buildTree e2
sameSymbols _ = False

isCommutative :: (Show a, Name a) => Expr a -> Bool
isCommutative (Quant _ _ _ (Builtin Equal :@: [e1, e2])) = isCommutative' e1 e2
isCommutative _ = False

isCommutative' :: (Show a, Name a) => Expr a -> Expr a -> Bool
isCommutative' g1@(Gbl (Global name1 _ _) :@: [f1]) g2@(Gbl (Global name2 _ _) :@: [f2]) = areTheSame g1 g2 && isCommutative' f1 f2
isCommutative' g1@(Gbl (Global name1 _ _) :@: [x1,x2]) g2@(Gbl (Global name2 _ _) :@: [y1,y2]) = areTheSame g1 g2 && areTheSame x1 y2 && areTheSame x2 y1
isCommutative' _ _ = False

isAssociative :: (Show a, Name a) => Expr a -> Bool
isAssociative (Quant _ _ _ (Builtin Equal :@: [e1, e2])) = isAssociative' e1 e2
isAssociative _ = False

isAssociative' :: (Show a, Name a) => Expr a -> Expr a -> Bool
isAssociative' (Gbl (Global name1 _ _) :@: [fab,c]) (Gbl (Global name2 _ _) :@: [a,fbc])
    | (isFunction fab 2) && (isFunction fbc 2) = name1 == name2 && namefab == namefbc && namefab == name1 && argumentsOk
    | otherwise = False
    where
        argumentsOk = (areTheSame a a') && (areTheSame b b') && (areTheSame c c')
        (Gbl (Global namefab _ _) :@: [a', b]) = fab
        (Gbl (Global namefbc _ _) :@: [b',c']) = fbc
isAssociative' _ _ = False

isFunction :: (Show a, Name a) => Expr a -> Int -> Bool
isFunction (Gbl (Global _ _ _) :@: args) len = (length args == len)
isFunction _ _ = False

areTheSame :: (Show a, Name a) => Expr a -> Expr a -> Bool
areTheSame (Gbl (Global name1 _ _) :@: [x]) (Gbl (Global name2 _ _) :@: [y]) = name1 == name2
areTheSame (Gbl (Global name1 _ _) :@: [x1,x2]) (Gbl (Global name2 _ _) :@: [y1,y2]) = name1 == name2
areTheSame (Lcl (Local name1 (TyCon _ _))) (Lcl (Local name2 (TyCon _ _))) = name1 == name2
areTheSame (Lcl (Local name1 (TyVar _))) (Lcl (Local name2 (TyVar _))) = name1 == name2
areTheSame _ _ = False

analyseSymbolicFunctionFeatures :: [(String, [String])] -> IO ([(String, [String])])
analyseSymbolicFunctionFeatures [] = return []
analyseSymbolicFunctionFeatures ((fName, features):xs) = do
    fs <- analyseSymbolic [(fName, features)]
    let (_, features') = head fs

    let rec = isRecursive fName features -- is the function recursive? Just looking for the function name among the features

    rest <- analyseSymbolicFunctionFeatures xs
    return $ (fName, concat $ [features', ["recursive " ++ (show rec)]]):rest

isRecursive :: String -> [String] -> Bool
isRecursive _ [] = False
isRecursive f' (f:fs)
    | f' == f = True
    | otherwise = isRecursive f' fs

most :: [(Int, String)] -> String
most list = f
    where
        (_,f) = head $ sortBy mostSorter list

mostSorter :: (Int, String) -> (Int, String) -> Ordering
mostSorter (a,_) (b,_) 
    | a > b = LT
    | a < b = GT
    | otherwise = EQ

freq :: [String] -> [(Int, String)]
freq l = map (\i -> (length i, head i)) (group (sort l))

intdiv :: Int -> Int -> Float
intdiv a b = (fromIntegral a) / (fromIntegral b)