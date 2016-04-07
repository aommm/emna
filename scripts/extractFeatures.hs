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
                    --conn <- connectPostgreSQL (pack "dbname='hipspec' user='hipspecuser' password='hipspecpassword'")
                    --clearDB conn
                    --insertLemmas conn (M.elems ls)

                    -- Extracting features
                    -- features <- getLemmaSymbols lib 2
                    abstractLemmas <- getAbstractFunctions lib 4
                    printList abstractLemmas
                    -- insertFeatures conn features
                    putStrLn "finished"

printList :: [(String, [String])] -> IO ()
printList ((lemma, []):xss) = do
    printList xss
printList ((lemma, (f:fs)):xss) = do
    putStrLn $ lemma ++ " = " ++ f
    printList ((lemma, fs):xss)
printList [] = return ()