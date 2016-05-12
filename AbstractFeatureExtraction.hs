{-# LANGUAGE OverloadedStrings #-}

module AbstractFeatureExtraction where

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

import FeatureExtraction

-- Going through each lemma of the library
getAbstractLemmas :: (Show a, Name a) => Map String (Formula a) -> Int -> [(String, [Feat])]
getAbstractLemmas ls depth 
    | null ls = []
    | otherwise = (name, features):rest
        where
            rest = getAbstractLemmas (M.fromList $ zip ks xs) depth
            (f:xs) = M.elems ls
            (k:ks) = M.keys ls
            name = case getFmName f of
                Nothing -> "current"
                Just nome -> nome
            tree = buildTree (fm_body f)
            trees = extractSubTrees depth tree
            features = map (\h -> (h, "la")) $ concat $ map extractFeatures trees

-- Going through each lemma of the library
getAbstractFunctions :: (Show a, Name a) => Map a (Function a) -> Int -> [(String, [Feat])]
getAbstractFunctions fs depth 
    | null fs = []
    | otherwise = (name, features):rest
        where
            rest = getAbstractFunctions (M.fromList $ zip ks xs) depth
            (f:xs) = M.elems fs
            (k:ks) = M.keys fs
            name = varStr $ func_name f
            tree = buildTree (func_body f)
            trees = extractSubTrees depth tree
            features = map (\h -> (h, "fa")) $ concat $ map extractFeatures trees

-- Builds a tree for an expression, recursively :)
buildTree :: (Show a, Name a) => Expr a -> FNode String
buildTree (Quant _ _ _ e') = buildTree e'
buildTree (Builtin Equal :@: exps) = FNode "Equals" $ map buildTree exps
buildTree (Builtin (Lit _) :@: _) = FNode "Const" []
buildTree (Builtin At :@: exps) = FNode "@" $ map buildTree exps
buildTree (Builtin Implies :@: exps) = FNode "=>" $ map buildTree exps
buildTree (Builtin Not :@: exps) = buildTree (head exps)

buildTree (Lcl (Local name (TyCon feature _))) = FNode "Var" []
buildTree (Lcl (Local name (TyVar feature))) = FNode "Var" []
buildTree l@(Lcl (Local name (ts :=>: t))) = FNode "FuncType" []

buildTree (Match e cases) = FNode ("match") $ map buildTree $ map case_rhs cases'
    where
        cases' = filter caseFilter cases
buildTree (Gbl (Global name typ args) :@: exps) 
    | varStr name == "nil" = FNode "Const" []
    | varStr name == "one" = FNode "Const" []
    | varStr name == "Z" = FNode "Const" []
    | otherwise = FNode "Func" $ map buildTree exps
buildTree (Lam locals e) = FNode "Lambda" [buildTree e]

buildTree e = trace (show e) $ FNode "unknown" []

caseFilter :: (Show a, Name a) => Case a -> Bool
caseFilter c@(Case (ConPat (Global id _ _) _) _) 
    | varStr id == "Z" = False
    | varStr id == "nil" = False
    | otherwise = True
caseFilter c = True

-- Gets string from expression
exprToString :: (Show a, Name a) => Expr a -> String
exprToString (Lcl (Local name _)) = "Var"
exprToString (Gbl (Global name typ args) :@: exps) = "Func"
exprToString (Builtin At :@: _) = "@"
exprToString b = "unknown: " ++ show b