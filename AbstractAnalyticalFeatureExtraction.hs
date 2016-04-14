{-# LANGUAGE OverloadedStrings #-}

module AbstractAnalyticalFeatureExtraction where

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
import qualified AbstractFeatureExtraction as AF

-- Quick analysis of a feature set
analyseAbstract :: [(String, [String])] -> IO ([(String, [String])])
analyseAbstract [] = return []
analyseAbstract ((lemmaName, features):xs) = do
    let nFeats = length features -- number of features
    let nDistFeats = length $ nub features -- number of distinct features

    let f' = ["_length " ++ (show nFeats), "_lengthDistinct " ++ (show nDistFeats)]

    rest <- analyseAbstract xs
    return $ (lemmaName, f'):rest

analyseAbstractLemmaFeatures :: (Show a, Name a) => [(String, [String])] -> Map String (Formula a) -> IO ([(String, [String])])
analyseAbstractLemmaFeatures [] _ = return []
analyseAbstractLemmaFeatures ((lemmaName, features):xs) ls = do

    let iFDepth = innerFunctionDepth (fm_body $ fromJust $ M.lookup lemmaName ls)
    let iF = iFDepth > 1

    rest <- analyseAbstractLemmaFeatures xs ls
    return $ (lemmaName, ["_innerFunctionApplication " ++ (show iF), "_innerFunctionDepth " ++ (show iFDepth)]):rest

analyseAbstractFunctionFeatures :: (Show a, Name a) => [(String, [String])] -> Map a (Function a) -> IO ([(String, [String])])
analyseAbstractFunctionFeatures [] _ = return []
analyseAbstractFunctionFeatures ((funcName, features):xs) fs = do
    let (_,function) = fromJust $ find (\(f,_) -> (varStr f) == funcName) (M.toList fs)

    let nArgs = numberOfArgs function

    rest <- analyseAbstractFunctionFeatures xs fs
    return $ (funcName, ["_nArgs " ++ (show nArgs)]):rest

numberOfArgs :: (Show a, Name a) => Function a -> Int
numberOfArgs f = length $ func_args f

innerFunctionDepth :: (Show a, Name a) => Expr a -> Int
innerFunctionDepth (Quant _ _ _ (Builtin Equal :@: [e1, e2])) = maximum [lhs, rhs]
    where
        lhs = innerFunctionDepth e1
        rhs = innerFunctionDepth e2

innerFunctionDepth (Gbl (Global name typ args) :@: []) = 1
innerFunctionDepth (Gbl (Global name typ args) :@: exps) = 1 + (maximum $ map innerFunctionDepth exps)
innerFunctionDepth _ = 0