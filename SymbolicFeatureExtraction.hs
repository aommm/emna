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
getLemmaSymbols :: (Show a, Name a) => Library a -> Int -> IO ([(String, [String])])
getLemmaSymbols (Library fs dts ls) depth 
    | null ls = do return []
    | otherwise = do
    let (f:xs) = M.elems ls
    let (k:ks) = M.keys ls
    let name = fromJust $ getFmName f

    let tree = buildTree (fm_body f)
    let trees = extractSubTrees depth tree
    let features = concat $ map extractFeatures trees

    rest <- getLemmaSymbols (Library fs dts (M.fromList $ zip ks xs)) depth
    return $ (name, features):rest

-- Going through each function of the library
getFunctionSymbols :: (Show a, Name a) => Library a -> Int -> IO ([(String, [String])])
getFunctionSymbols (Library fs dts ls) depth 
    | null fs = do return []
    | otherwise = do
    let (f:xs) = M.elems fs
    let (k:ks) = M.keys fs
    let name = varStr $ func_name f
    
    let tree = buildTree (func_body f)
    let trees = extractSubTrees depth tree
    let features = nub $ concat $ map extractFeatures trees

    rest <- getFunctionSymbols (Library (M.fromList $ zip ks xs) dts ls) depth
    return $ (name, features):rest
    
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