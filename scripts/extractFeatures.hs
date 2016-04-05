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


-- Reads a library file to begin with
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    connString <- getConnString
    exists <- doesFileExist filePath
    case exists of
        False -> putStrLn "File not found"
        True -> do
            libraryString <- readFile filePath
            case parseLibrary libraryString of
                Left msg      -> error $ "Parsing library failed:"++show msg
                Right (Library _ _ ls) -> do
                    features <- formulasToFeatures (M.elems ls)
                    conn <- connectPostgreSQL (pack "dbname='hipspec' user='' password=''")
                    clearDB conn
                    insertLemmas conn features
                    insertFeatures conn features
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

