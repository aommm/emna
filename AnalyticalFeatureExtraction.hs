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
analyseSymbolic :: [(String, [Feat])] -> [(String, [String])]
analyseSymbolic [] = []
analyseSymbolic ((lemmaName, features):xs) = (lemmaName, f'):rest
    where
        rest = analyseSymbolic xs
        nFeats = length features -- number of features
        nDistFeats = length $ nub features -- number of distinct features
        ratio = intdiv nDistFeats nFeats -- 
        (mostPopular,_) = trace (lemmaName ++ " " ++ show features) $ most (freq features) -- most popular feature of the lemma
        f' = ["length " ++ (show nFeats), "lengthDistinct " ++ (show nDistFeats), "distinctRatio " ++ (printf "%.1f" $ ratio), "popular " ++ mostPopular]

analyseSymbolicLemmaFeatures :: (Show a, Name a) => [(String, [Feat])] -> Map String (Formula a) -> [(String, [Feat])]
analyseSymbolicLemmaFeatures [] _ = []
analyseSymbolicLemmaFeatures ((lemmaName, features):xs) ls = (lemmaName, map (\s -> (s, "als")) $ features' ++ (getBooleanFeatures [commutative, associative])):rest
    where
        rest = analyseSymbolicLemmaFeatures xs ls
        fs = analyseSymbolic [(lemmaName, features)]
        (_, features') = head fs
        same = sameSymbols (fm_body $ fromJust $ M.lookup lemmaName ls)
        exactSame = exactSameSymbols (fm_body $ fromJust $ M.lookup lemmaName ls)
        commutative = ("commutative", (if exactSame then (isCommutative (fm_body $ fromJust $ M.lookup lemmaName ls)) else False))
        associative = ("associative", (if exactSame then (isAssociative (fm_body $ fromJust $ M.lookup lemmaName ls)) else False))
        -- mainF = mainFunction (fm_body $ fromJust $ M.lookup lemmaName ls)
  
analyseSymbolicFunctionFeatures :: [(String, [Feat])] -> [(String, [Feat])]
analyseSymbolicFunctionFeatures [] = []
analyseSymbolicFunctionFeatures ((fName, features):xs) = (fName, map (\s -> (s, "afs")) $ features'):rest
    where
        rest = analyseSymbolicFunctionFeatures xs
        fs = analyseSymbolic [(fName, features)]
        (_, features') = head fs
getBooleanFeatures :: [(String, Bool)] -> [String]
getBooleanFeatures ls = map (\(y,b) -> y) $ filter (\(y,b) -> b) ls

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
    | (isFunctionWithArgs fab 2) && (isFunctionWithArgs fbc 2) = name1 == name2 && namefab == namefbc && namefab == name1 && argumentsOk
    | otherwise = False
    where
        argumentsOk = (areTheSame a a') && (areTheSame b b') && (areTheSame c c')
        (Gbl (Global namefab _ _) :@: [a', b]) = fab
        (Gbl (Global namefbc _ _) :@: [b',c']) = fbc
isAssociative' _ _ = False

isFunctionWithArgs :: (Show a, Name a) => Expr a -> Int -> Bool
isFunctionWithArgs (Gbl (Global _ _ _) :@: args) len = (length args == len)
isFunctionWithArgs _ _ = False

isFunction :: (Show a, Name a) => Expr a -> Bool
isFunction (Gbl (Global _ _ _) :@: _) = True
isFunction _ = False

areTheSame :: (Show a, Name a) => Expr a -> Expr a -> Bool
areTheSame (Gbl (Global name1 _ _) :@: [x]) (Gbl (Global name2 _ _) :@: [y]) = name1 == name2
areTheSame (Gbl (Global name1 _ _) :@: [x1,x2]) (Gbl (Global name2 _ _) :@: [y1,y2]) = name1 == name2
areTheSame (Lcl (Local name1 (TyCon _ _))) (Lcl (Local name2 (TyCon _ _))) = name1 == name2
areTheSame (Lcl (Local name1 (TyVar _))) (Lcl (Local name2 (TyVar _))) = name1 == name2
areTheSame _ _ = False

isRecursive :: String -> [Feat] -> Bool
isRecursive _ [] = False
isRecursive f' ((f,_):fs)
    | f' == f = True
    | otherwise = isRecursive f' fs

most :: [(Int, Feat)] -> Feat
most list = f
    where
        (_,f) = head $ sortBy mostSorter list

mostSorter :: (Int, Feat) -> (Int, Feat) -> Ordering
mostSorter (a,_) (b,_) 
    | a > b = LT
    | a < b = GT
    | otherwise = EQ

freq :: [Feat] -> [(Int, Feat)]
freq l = map (\i -> (length i, head i)) (group (sort l))

intdiv :: Int -> Int -> Float
intdiv a b = (fromIntegral a) / (fromIntegral b)