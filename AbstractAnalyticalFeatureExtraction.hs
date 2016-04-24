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
analyseAbstract :: [(String, [String])] -> [(String, [String])]
analyseAbstract [] = []
analyseAbstract ((lemmaName, features):xs) = (lemmaName, f'):rest
    where
        rest = analyseAbstract xs
        nFeats = length features -- number of features
        nDistFeats = length $ nub features -- number of distinct features
        f' = ["_abstractLength " ++ (show nFeats), "_abstractLengthDistinct " ++ (show nDistFeats)]

analyseAbstractLemmaFeatures :: (Show a, Name a) => [(String, [String])] -> Map String (Formula a) -> [(String, [String])]
analyseAbstractLemmaFeatures [] _ = []
analyseAbstractLemmaFeatures ((lemmaName, features):xs) ls = (lemmaName, map (\s -> "_ala " ++ s) $ f' ++ ["_innerFunctionApplication " ++ (show iF), "_innerFunctionDepth " ++ (show iFDepth)]):rest
    where
        rest = analyseAbstractLemmaFeatures xs ls
        [(name, f')] = analyseAbstract [(lemmaName, features)]
        iFDepth = innerFunctionDepth (fm_body $ fromJust $ M.lookup lemmaName ls)
        iF = iFDepth >= 2
    
analyseAbstractFunctionFeatures :: (Show a, Name a) => [(String, [String])] -> Map a (Function a) -> [(String, [String])]
analyseAbstractFunctionFeatures [] _ = []
analyseAbstractFunctionFeatures ((funcName, features):xs) fs = (funcName, map (\s -> "_afa " ++ s) $ f' ++ ["_nArgs " ++ (show nArgs)]):rest
    where
        rest = analyseAbstractFunctionFeatures xs fs
        (_,function) = fromJust $ find (\(f,_) -> (varStr f) == funcName) (M.toList fs)
        [(name, f')] = analyseAbstract [(funcName, features)]
        nArgs = numberOfArgs function
    

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