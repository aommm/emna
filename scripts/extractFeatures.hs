{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import Tip.Parser (parseLibrary)
import Tip.Parser.Convert
import Tip.Library
import Data.Map (Map)
import qualified Data.Map as M
import Tip.Types
import Tip.Fresh
import Tip.Pretty
import Data.Maybe
import Data.List
import Debug.Trace
import Control.Monad
import Database.PostgreSQL.Simple
import Data.ByteString.Internal
import Data.ByteString.Char8 (pack)
import Data.Char

import FeatureExtraction
import SymbolicFeatureExtraction
import AbstractFeatureExtraction
import AbstractAnalyticalFeatureExtraction
import AnalyticalFeatureExtraction

-- Reads a library file to begin with
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    let depth = head $ tail args
    let schemes = drop 2 args

    exists <- doesFileExist filePath
    case exists of
        False -> putStrLn "File not found"
        True -> do
            libraryString <- readFile filePath
            case parseLibrary libraryString of
                Left msg      -> error $ "Parsing library failed:"++show msg
                Right lib@(Library fs _ ls) -> do
                    -- Prepping the database
                    conn <- connectPostgreSQL (pack "dbname='hipspec' user='hipspecuser' password='hipspecpassword'")
                    clearDB conn
                    insertLemmas conn (M.elems ls)

                    let extractionSchemes = getExtractionSchemes schemes
                    let visibleExtractionSchemes = filter (\s -> s `elem` schemes) extractionSchemes
                    let analyticalSchemes = filter (\(x:xs) -> x == 'a') schemes
                    let iDepth = (digitToInt (head depth))

                    let features = runExtractionSchemes extractionSchemes iDepth lib

                    let totalFeatures = runAnalyticSchemes analyticalSchemes features iDepth lib

                    let lemmaFeats = map (\(k,v) -> v) $ filter (\(k,_) -> k `elem` ["ls", "la", "ala", "als"] && (k `elem` visibleExtractionSchemes || k `elem` ["ala", "als"] || k == "ls")) $ M.toList totalFeatures
                    let functionFeats = map (\(k,v) -> v) $ filter (\(k,_) -> k `elem` ["fs", "fa", "afa", "afs"] && (k `elem` visibleExtractionSchemes || k `elem` ["afa", "afs"])) $ M.toList totalFeatures

                    let lemmaNames = map (\l -> fromJust $ getFmName l) (M.elems ls)

                    let firstHalf = (if length lemmaFeats == 0 then (emptyLemmaList lemmaNames) else (generateHalf lemmaFeats))
                    let secondHalf = generateHalf functionFeats

                    let finalFeatures = mergeFeatures ("ls" `elem` schemes) firstHalf secondHalf

                    printList finalFeatures

                    insertFeatures conn finalFeatures
                    putStrLn "finished"

emptyLemmaList :: [String] -> [(String, [String])]
emptyLemmaList ls = zip ls (take (length ls) (repeat []))

generateHalf :: [[(String, [String])]] -> [(String, [String])]
generateHalf [] = []
generateHalf ls = foldl preMerge (head ls) (tail ls)

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

printList :: [(String, [String])] -> IO ()
printList ((lemma, []):xss) = do
    putStrLn ""
    printList xss
printList ((lemma, (f:fs)):xss) = do
    putStrLn $ lemma ++ " = " ++ f
    printList ((lemma, fs):xss)
printList [] = return ()

preMerge :: [(String, [String])] -> [(String, [String])] -> [(String, [String])]
preMerge xs ys = preMerge' (sort xs) (sort ys) -- We can assume the lists are equally long here

preMerge' :: [(String, [String])] -> [(String, [String])] -> [(String, [String])]
preMerge' xs ys = map (\((n, f1),(_, f2)) -> (n, nub $ f1 ++ f2)) $ zip xs ys

-- Not sure about the complexity of this one hehe :-)
mergeFeatures :: Bool -> [(String, [String])] -> [(String, [String])] -> [(String, [String])]
mergeFeatures sslf ((n, feats):ls) fs = (n, extendedFeats):(mergeFeatures sslf ls fs)
    where
        extendedFeats = (filter (\y -> (not sslf && not (isSymbolicLemmaFeat y)) || sslf) feats) ++ (nub addedFeats)
        addedFeats = concat $ map (\(n', f) -> map (\(f') -> ("_func " ++ f')) f) funcsOfLemma
        funcsOfLemma = filter (\(n', _) -> any (\featString -> (featString == "_s_ " ++ n')) feats) fs -- finding the function features which has its key anywhere in the features of the lemma
mergeFeatures _ [] _ = []

isSymbolicLemmaFeat :: String -> Bool
isSymbolicLemmaFeat ('_':'s':'_':xs) = True
isSymbolicLemmaFeat _ = False