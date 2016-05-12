{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
-- module Librarify where

import Tip.HaskellFrontend
import Tip.QuickSpec
import Tip.Core hiding (Unknown)
import Tip.Library
import Tip.Fresh
import Tip.Passes
import Tip.Pretty
import Tip.Scope
import Tip.Pretty.SMT as SMT
import Tip.Parser.PrintTIP (render)
import Tip.Parser (parseLibrary)
import Tip.Rename

import Debug.Trace

import Tip.Utils.Rename
import Tip.Utils (usort)

import Data.Generics.Geniplate

import Data.Maybe
import Data.List (sort, sortBy, isInfixOf, nub, (\\), intercalate, delete)
import Data.Ord
import Data.Char

import qualified Data.Traversable as T

import Data.Unique

import Control.Monad

import Control.Concurrent.STM.Promise
import Control.Concurrent.STM.Promise.Process
import Control.Concurrent.STM.Promise.Tree hiding (Node)
import Control.Concurrent.STM.Promise.Workers

import Text.PrettyPrint (Doc)
import Text.Read (readMaybe)

import System.Environment
import System.Environment.FindBin
import System.Directory

import Waldmeister

import qualified System.IO as IO
import System.Console.CmdArgs
import System.Process (readProcessWithExitCode, readCreateProcess, CmdSpec(RawCommand), CreateProcess(cwd,CreateProcess), proc )
import System.Directory (makeAbsolute, copyFile)
import System.FilePath.Posix (takeBaseName, replaceBaseName)

import Utils
import ExtractionPoint (runSchemesConjecture)

import qualified Data.Map as M
import Data.Map (Map)

--import Main

data Args =
  Args
    { file    :: String
    , explore :: Bool
    , extra   :: [String]
    , indvars :: Int
    , timeout :: Double
    , filenames :: Bool
    , prover    :: String
    , output    :: String
    , workingDir :: String
    , dataDir   :: String
    }
  deriving (Show,Data,Typeable)

defArgs :: Args
defArgs =
  Args
    { file    = ""    &= argPos 0 &= typFile
    , explore = True  &= name "e" &= help "Explore theory"
    , extra   = []                &= help "Additional functions for exploration"
    , indvars = 1     &= name "v" &= help "Number of variables to do induction on"
    , timeout = 1     &= name "t" &= help "Timeout in seconds (default 1)"
    , filenames = False           &= help "Print out filenames of theories"
    , prover = "w"                &= help "Prover (waldmeister/z3)"
    , output = "" &= name "o" &= help "Proof output file (default: dataDir/lib.tiplib)"
    -- v Workaround: store cwd here since getCurrentDirectory crashes in some fns
    , workingDir = ""         &= help "Working directory for scripts (default: cwd)" 
    , dataDir = "" &= name "d" &= help "Path to data directory (default: cwd/data)"
    }
  &= program "emna" &= summary "simple inductive prover with proof output"

main :: IO ()
main = do  
  args@Args{..} <- parseArgs
  x <- readHaskellOrTipFile file defaultParams{ extra_names = extra }
  case x of
    Left err  -> putStrLn err
    Right thy -> do
      let theory = ren thy
      let theory2 = theory{ thy_asserts = (map fixLemma (thy_asserts theory)) }
      saveTheory args theory2
  where
    ren = renameWith (\ x -> [ I i (varStr x) | i <- [0..] ])

data I = I Int String
  deriving (Eq,Ord,Show)

instance PrettyVar I where
  varStr (I x s) = s

instance Name I where
  fresh             = refresh (I undefined "x")
  refresh (I _ s)   = do u <- fresh; return (I u s)
  freshNamed s      = refresh (I undefined s)
  getUnique (I u _) = u

fixLemma :: Formula a -> Formula a
fixLemma (Formula role (UserAsserted str) types body) = Formula role (Lemma 0 (Just "sune") sketch) types body
  where
    sketch = Just (ProofSketch [] [] Structural "herp" "derp")

-- Save library to file
saveTheory :: (Show a, Name a,Ord a) => Args -> Theory a -> IO ()
saveTheory args thy = do
  when (hasOutput args) $ do

    -- TODO: only absolute path works here, why!?
    -- http://stackoverflow.com/questions/21765570/haskell-compilation-with-an-input-file-error-openfile-does-not-exist-no-such
    let filePath = output args
    -- maybe read existing library
    exists <- doesFileExist filePath
    library <- if exists
                 then do
                   libraryString <- readFile filePath
                   case parseLibrary libraryString of
                    Right library -> putStrLn "loaded library" >> return library
                    Left msg      -> error $ "parsing library failed:"++show msg
                 else
                   putStrLn "new library" >> return emptyLibrary

    -- library -> theory, so that we can renameAvoiding
    let libraryThy = libToThy library -- :: Theory Id
        libraryThy' = renameAvoiding SMT.smtKeywords SMT.validSMTChar libraryThy :: Theory RenamedId
        library' = thyToLib libraryThy' :: Library RenamedId
        thy' = renameAvoiding SMT.smtKeywords SMT.validSMTChar thy :: Theory RenamedId

        library'' = extendLibrary thy' library'
        libString = ppRender library''

    --putStrLn "library reverse" -- type variable here is named 'a2' instead of 'a', causing inequality
    --print $ id $ (thy_funcs libraryThy') !! 1
    --putStrLn "theory reverse"
    --print $ id $ (thy_funcs thy') !! 1

    putStrLn $ "saving theory to "++ filePath ++ "..."
    writeFile filePath libString
    putStrLn "... done!"

    let newName = (takeBaseName filePath) ++ "-" ++ (takeBaseName $ file args)
    let filePath' = replaceBaseName filePath newName
    putStrLn $ "saving backup of theory to "++ filePath' ++ "..."
    writeFile filePath' libString
    putStrLn "... done!"

    return ()
  where hasOutput _ = True

-- Parse command line arguments
parseArgs :: IO Args
parseArgs = do
  cwd <- getCurrentDirectory 
  args <- cmdArgs defArgs
  -- Process 'dataDir'
  let dataDir' = if dataDir args == ""
                  then cwd ++ "/data"
                  else dataDir args
  -- Process 'output'
  let output' = if output args == ""
                 then dataDir' ++ "/lib.tiplib"
                 else output args

  -- make paths absolute
  -- dereference hej to path/to/cwd/hej
  -- TODO: doesn't defererence ~, . or ..
  dataDir'' <- makeAbsolute dataDir'
  output'' <- makeAbsolute output'
  return args {output = output'', dataDir = dataDir'', workingDir = cwd}
