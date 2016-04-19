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

-- Runs all the schemes on a set of lemmas and functions
formulasToFeatures :: (Show a, Name a) => [Formula a] -> Library a -> IO ([(String, [String])])
formulasToFeatures ls lib = do
    let iDepth = 3
    let features = runExtractionSchemes ["la","ls","fs","fa"] iDepth lib
    let totalFeatures = runAnalyticSchemes ["ala","als","afs","afa"] features iDepth lib
    let visibleExtractionSchemes = ["la","ls","fs","fa"]

    let lemmaFeats = map (\(k,v) -> v) $ filter (\(k,_) -> k `elem` ["ls", "la", "ala", "als"] && (k `elem` visibleExtractionSchemes || k `elem` ["ala", "als"] || k == "ls")) $ M.toList totalFeatures
    let functionFeats = map (\(k,v) -> v) $ filter (\(k,_) -> k `elem` ["fs", "fa", "afa", "afs"] && (k `elem` visibleExtractionSchemes || k `elem` ["afa", "afs"])) $ M.toList totalFeatures

    let lemmaNames = map (\l -> fromJust $ getFmName l) ls

    let firstHalf = (if length lemmaFeats == 0 then (emptyLemmaList lemmaNames) else (generateHalf lemmaFeats))
    let secondHalf = generateHalf functionFeats

    return $ mergeFeatures True firstHalf secondHalf

runAnalyticSchemes :: (Show a, Name a) => [String] -> Map String [(String, [String])] -> Int -> Library a -> Map String [(String, [String])]
runAnalyticSchemes ("afa":xs) feats depth lib@(Library fs _ ls) = M.insert "afa" (analyseAbstractFunctionFeatures (fromJust $ M.lookup "fa" feats) fs) (runAnalyticSchemes xs feats depth lib)
runAnalyticSchemes ("afs":xs) feats depth lib@(Library fs _ ls) = M.insert "afs" (analyseSymbolicFunctionFeatures (fromJust $ M.lookup "fs" feats)) (runAnalyticSchemes xs feats depth lib)
runAnalyticSchemes ("ala":xs) feats depth lib@(Library fs _ ls) = M.insert "ala" (analyseAbstractLemmaFeatures (fromJust $ M.lookup "la" feats) ls) (runAnalyticSchemes xs feats depth lib)
runAnalyticSchemes ("als":xs) feats depth lib@(Library fs _ ls) = M.insert "als" (analyseSymbolicLemmaFeatures (fromJust $ M.lookup "ls" feats) ls) (runAnalyticSchemes xs feats depth lib)
runAnalyticSchemes [] feats _ _ = feats

runExtractionSchemes :: (Show a, Name a) => [String] -> Int -> Library a -> Map String [(String, [String])]
runExtractionSchemes ("fs":xs) depth lib = M.insert "fs" (getFunctionSymbols lib depth) (runExtractionSchemes xs depth lib)
runExtractionSchemes ("fa":xs) depth lib = M.insert "fa" (getAbstractFunctions lib depth) (runExtractionSchemes xs depth lib)
runExtractionSchemes ("la":xs) depth lib = trace ("getting abstract lemmas") $ M.insert "la" (getAbstractLemmas lib depth) (runExtractionSchemes xs depth lib)
runExtractionSchemes ("ls":xs) depth lib = M.insert "ls" (getLemmaSymbols lib depth) (runExtractionSchemes xs depth lib)
runExtractionSchemes _ _ _ = M.empty

getExtractionSchemes :: [String] -> [String]
getExtractionSchemes ("fa":xs) = nub $ ("fa"):(getExtractionSchemes xs)
getExtractionSchemes ("fs":xs) = nub $ ("fs"):(getExtractionSchemes xs)
getExtractionSchemes ("la":xs) = nub $ ("la"):(getExtractionSchemes xs)
getExtractionSchemes ("ls":xs) = nub $ ("ls"):(getExtractionSchemes xs)

getExtractionSchemes ("ala":xs) = nub $ ("la"):(getExtractionSchemes xs)
getExtractionSchemes ("afs":xs) = nub $ ("fs"):("ls"):(getExtractionSchemes xs)
getExtractionSchemes ("afa":xs) = nub $ ("fa"):(getExtractionSchemes xs)
getExtractionSchemes ("als":xs) = nub $ ("ls"):(getExtractionSchemes xs)

getExtractionSchemes (_:xs) = getExtractionSchemes xs
getExtractionSchemes [] = []