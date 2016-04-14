{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import Tip.Parser (parseLibrary)
import Tip.Parser.Convert
import Tip.Library
import Data.Map (Map)
import qualified Data.Map as M
import Tip.Types
import Tip.Pretty
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

-- Reads a library file to begin with
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
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

                    -- Extracting features
                    let depth = 2
                    lemmaSymbols <- getLemmaSymbols lib depth
                    lemmaAbstract <- getAbstractLemmas lib depth
                    functionSymbols <- getFunctionSymbols lib depth
                    functionAbstract <- getAbstractFunctions lib depth
                    
                    putStrLn "\n\n\n\n\n\n\n\n\n"

                    analyticalLemmaSymbols <- analyseSymbolicLemmaFeatures lemmaSymbols ls
                    analyticalLemmaAbstracts <- analyseAbstractLemmaFeatures lemmaAbstract ls
                    analyticalFunctionSymbols <- analyseSymbolicFunctionFeatures functionSymbols
                    analyticalFunctionAbstracts <- analyseAbstractFunctionFeatures functionAbstract fs

                    let lemma = preMerge lemmaSymbols $ preMerge lemmaAbstract $ preMerge analyticalLemmaSymbols analyticalLemmaAbstracts
                    let function = preMerge functionSymbols $ preMerge functionAbstract $ preMerge analyticalFunctionSymbols analyticalFunctionAbstracts

                    let features = mergeFeatures lemma function

                    printList features

                    insertFeatures conn features
                    putStrLn "finished"

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
mergeFeatures :: [(String, [String])] -> [(String, [String])] -> [(String, [String])]
mergeFeatures ((n, feats):ls) fs = (n, extendedFeats):(mergeFeatures ls fs)
    where
        extendedFeats = feats ++ (nub addedFeats)
        addedFeats = concat $ map (\(n', f) -> map (\(f') -> ("_func " ++ f')) f) funcsOfLemma
        funcsOfLemma = filter (\(n', _) -> any (\featString -> featString == n') feats) fs -- finding the function features which has its key anywhere in the features of the lemma
mergeFeatures [] _ = []