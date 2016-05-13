{-# LANGUAGE OverloadedStrings #-}

module ExtractionPoint where

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
import SymbolicFeatureExtraction
import AbstractFeatureExtraction
import AbstractAnalyticalFeatureExtraction
import AnalyticalFeatureExtraction

runSchemesLibrary :: (Show a, Name a) => Map String (Formula a) -> Map a (Function a) -> [String] -> Int -> IO ([(String, [Feat])])
runSchemesLibrary fms funcs schemes depth = do
    let ls = getLemmaSymbols fms depth

    let la = (if (elem "la" schemes || elem "ala" schemes) then (getAbstractLemmas fms depth) else [])
    let fs = (if (elem "fs" schemes || elem "afs" schemes) then (getFunctionSymbols funcs depth) else [])
    let fa = (if (elem "fa" schemes || elem "afa" schemes) then (getAbstractFunctions funcs depth) else [])

    let als = (if (elem "als" schemes) then (analyseSymbolicLemmaFeatures ls fms) else [])
    let ala = (if (elem "ala" schemes) then (analyseAbstractLemmaFeatures la fms) else [])
    let afs = (if (elem "afs" schemes) then (analyseSymbolicFunctionFeatures fs) else [])
    let afa = (if (elem "afa" schemes) then (analyseAbstractFunctionFeatures fa funcs) else [])

    let lemmaFeats = map snd $ filter (\(k,_) -> elem k schemes || k == "ls") $ filter (\(_,v) -> length v > 0) [("ls",ls),("la",la),("als",als),("ala",ala)]
    let functionFeats = map snd $ filter (\(k,_) -> elem k schemes) $ filter (\(_,v) -> length v > 0) [("afs",afs),("afa",afa),("fs",fs),("fa",fa)]

    putStrLn $ "heheh = " ++ (show functionFeats)

    let lemmaNames = M.keys fms

    let firstHalf = (if length lemmaFeats == 0 then (emptyLemmaList lemmaNames) else (generateHalf lemmaFeats))
    let secondHalf = generateHalf functionFeats

    return $ mergeFeatures ("ls" `elem` schemes) firstHalf secondHalf

runFunctionSchemesLibrary :: (Show a, Name a) => Map String (Formula a) -> Map a (Function a) -> [String] -> Int -> IO ([(String, [Feat])])
runFunctionSchemesLibrary fms funcs schemes depth = do

    let fs = (if (elem "fs" schemes) then (getFunctionSymbols funcs depth) else [])
    let fa = (if (elem "fa" schemes) then (getAbstractFunctions funcs depth) else [])

    let functionFeats = map snd $ filter (\(k,_) -> elem k schemes) $ filter (\(_,v) -> length v > 0) [("fs",fs),("fa",fa)]

    return $ generateHalf functionFeats

runSchemesConjecture :: (Show a, Name a) => Formula a -> Map a (Function a) -> [String] -> Int -> IO ([Feat])
runSchemesConjecture f funcs schemes depth = do 
    list <- runSchemesLibrary (M.fromList [("current", f)]) funcs schemes depth
    return $ snd $ head $ list