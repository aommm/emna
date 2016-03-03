import System.Environment
import System.Directory
import Tip.Parser (parseLibrary)
import Tip.Library
import Data.Map (Map)
import qualified Data.Map as M
import Tip.Types
import Tip.Pretty

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
                Left msg      -> error $ "parsing library failed:"++show msg
                Right library -> do
                    putStrLn "loaded library"
                    saveLibrary library

saveLibrary :: (PrettyVar a, Show a, Ord a) => Library a -> IO ()
saveLibrary (Library _ _ ls) = do 
    readFeature $ M.elems ls

readFeature :: (PrettyVar a, Show a, Ord a) => [Formula a] -> IO ()
readFeature [] = return ()
readFeature (f:xs) = do
    let expression = fm_body f
    putStrLn $ show expression
    case expression of
        Quant _ _ local innerExpr -> do
            putStrLn $ show innerExpr
        _ -> do
            putStrLn ":("
    -- readFeature xs
