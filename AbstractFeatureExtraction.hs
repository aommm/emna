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
getAbstractLemmas :: (Show a, Name a) => Library a -> Int -> IO ([(String, [String])])
getAbstractLemmas (Library fs dts ls) depth 
    | null ls = do return []
    | otherwise = do
    let (f:xs) = M.elems ls
    let (k:ks) = M.keys ls
    let name = fromJust $ getFmName f

    let tree = buildTree (fm_body f)
    let trees = extractSubTrees depth tree
    let features = nub $ concat $ map extractFeatures trees

    rest <- getAbstractLemmas (Library fs dts (M.fromList $ zip ks xs)) depth
    return $ (name, features):rest

-- Going through each lemma of the library
getAbstractFunctions :: (Show a, Name a) => Library a -> Int -> IO ([(String, [String])])
getAbstractFunctions (Library fs dts ls) depth 
    | null fs = do return []
    | otherwise = do
    let (f:xs) = M.elems fs
    let (k:ks) = M.keys fs
    let name = varStr $ func_name f

    let tree = buildTree (func_body f)
    let trees = extractSubTrees depth tree
    let features = nub $ concat $ map extractFeatures trees

    rest <- getAbstractFunctions (Library (M.fromList $ zip ks xs) dts ls) depth
    return $ (name, features):rest

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

buildTree (Match e cases) = FNode ("match " ++ (exprToString e)) $ map buildTree $ map case_rhs cases
buildTree (Gbl (Global name typ args) :@: exps) = FNode "Func" $ map buildTree exps
buildTree (Lam locals e) = FNode "Lambda" [buildTree e]

buildTree e = trace (show e) $ FNode "unknown" []

-- Gets string from expression
exprToString :: (Show a, Name a) => Expr a -> String
exprToString (Lcl (Local name _)) = "Var"
exprToString (Gbl (Global name typ args) :@: exps) = "Func"
exprToString (Builtin At :@: _) = "@"
exprToString b = "unknown: " ++ show b