{-# LANGUAGE OverloadedStrings #-}

module SymbolicFeatureExtraction where

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
getLemmaSymbols :: (Show a, Name a) => Map String (Formula a) -> Int -> [(String, [String])]
getLemmaSymbols ls depth
    | null ls = []
    | otherwise = (name, features):rest
        where
            rest = getLemmaSymbols (M.fromList $ zip ks xs) depth
            (f:xs) = M.elems ls
            (k:ks) = M.keys ls
            name = k
            tree = buildTree (fm_body f)
            trees = extractSubTrees depth tree
            features = map (\y -> "_s_ " ++ y) $ concat $ map extractFeatures trees

-- Going through each function of the library
getFunctionSymbols :: (Show a, Name a) => Map a (Function a) -> Int -> [(String, [String])]
getFunctionSymbols fs depth 
    | null fs = []
    | otherwise = (name, features):rest
        where
            rest = getFunctionSymbols (M.fromList $ zip ks xs) depth
            (f:xs) = M.elems fs
            (k:ks) = M.keys fs
            name = varStr $ func_name f
            tree = buildTree (func_body f)
            trees = extractSubTrees depth tree
            features = nub $ concat $ map extractFeatures trees
    
-- Builds a tree for an expression, recursively :)
buildTree :: (Show a, Name a) => Expr a -> FNode String
buildTree (Quant _ _ _ e') = buildTree e'
buildTree (Builtin Equal :@: exps) = FNode "==" $ map buildTree exps
buildTree (Builtin (Lit (Bool b)) :@: _) = FNode (show b) []
buildTree (Builtin At :@: exps) = FNode "@" $ map buildTree exps
buildTree (Builtin Implies :@: exps) = FNode "=>" $ map buildTree exps
buildTree (Builtin Not :@: exps) = FNode "not" $ map buildTree exps

buildTree (Lcl (Local name (TyCon feature _))) = FNode (varStr feature) []
buildTree (Lcl (Local name (TyVar feature))) = FNode "anyType" []
buildTree l@(Lcl (Local name (ts :=>: t))) = FNode ((concat $ map typeToString ts) ++  (" :=>: " ++ typeToString t)) []

buildTree (Gbl (Global name typ args) :@: exps) = FNode (varStr name) $ map buildTree exps
buildTree (Match e cases) = FNode ("match " ++ (exprToString e)) $ map buildTree $ map case_rhs cases
buildTree (Lam locals e) = FNode "lambda" [buildTree e]

buildTree e = trace (show e) $ FNode "unknown" []

-- Gets string from expression
exprToString :: (Show a, Name a) => Expr a -> String
exprToString (Lcl (Local name _)) = varStr name
exprToString (Gbl (Global name typ args) :@: exps) = varStr name
exprToString (Builtin At :@: _) = "@"
exprToString b = "unknown: " ++ show b

-- Gets string from type
typeToString :: (Show a, Name a) => Type a -> String
typeToString (TyVar b) = varStr b
typeToString (BuiltinType b) = show b