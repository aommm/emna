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

import ExtractionPoint
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

    connString <- getConnString
    exists <- doesFileExist filePath
    case exists of
        False -> putStrLn "File not found"
        True -> do
            libraryString <- readFile filePath
            case parseLibrary libraryString of
                Left msg      -> error $ "Parsing library failed:"++show msg
                Right lib@(Library fs _ ls) -> do
                    -- Prepping the database
                    conn <- connectPostgreSQL (pack connString)
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


getConnString :: IO String
getConnString = do
    dbName <- lookupEnv "HS_DB_NAME"
    dbHost <- lookupEnv "HS_DB_HOST"
    dbUsername <- lookupEnv "HS_DB_USERNAME"
    dbPassword <- lookupEnv "HS_DB_PASSWORD"
    let dbName' = fromMaybe "hipspec" dbName
    let dbHost' = fromMaybe "localhost" dbHost
    let connString = "dbname='"++ dbName'++ "' host='"++ dbHost' ++"'"
    let connStringUser = maybe "" (\s -> " username='"++ s ++"'") dbUsername
    let connStringPass = maybe "" (\s -> " password='"++ s ++"'") dbPassword
    return $ connString ++ connStringUser ++ connStringPass