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
                    (Library fs' _ ls') <- filterNonInductiveLemmas lib
                    -- Prepping the database
                    putStrLn connString
                    conn <- connectPostgreSQL (pack connString)
                    clearDB conn
                    let thy = libToThy lib
                    insertLemmas conn (M.elems ls) thy

                    finals <- runSchemesLibrary ls fs schemes (digitToInt (head depth))
                    let finalFeatures = removeDuplicates finals
                    printList finalFeatures

                    insertFeatures conn finalFeatures
                    putStrLn "finished"

filterNonInductiveLemmas :: Library a -> IO (Library a)
filterNonInductiveLemmas (Library fs dts ls) = do
    let filteredLemmas = filter (\(name,(Formula _ (Lemma _ _ (Just ps)) _ _)) -> length (indVars ps) > 0) $ M.toList ls
    return (Library fs dts (M.fromList filteredLemmas))

getConnString :: IO String
getConnString = do
    dbName <- lookupEnv "HS_DB_NAME"
    dbHost <- lookupEnv "HS_DB_HOST"
    dbUsername <- lookupEnv "HS_DB_USERNAME"
    dbPassword <- lookupEnv "HS_DB_PASSWORD"
    let dbName' = fromMaybe "hipspec" dbName
    let dbHost' = fromMaybe "localhost" dbHost
    let connString = "dbname='"++ dbName'++ "' host='"++ dbHost' ++"'"
    let connStringUser = maybe "" (\s -> " user='"++ s ++"'") dbUsername
    let connStringPass = maybe "" (\s -> " password='"++ s ++"'") dbPassword
    return $ connString ++ connStringUser ++ connStringPass