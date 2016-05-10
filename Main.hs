{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
module Main where

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


main :: IO ()
main = do  
  args@Args{..} <- parseArgs
  x <- readHaskellOrTipFile file defaultParams{ extra_names = extra }
  case x of
    Left err  -> putStrLn err
    Right thy ->
      do let (rmb,p) = case prover of
                        'w':_ -> (True, waldmeister)
                        'z':_ -> (False,z3)
         l <- loop args p =<<
           ((if explore then exploreTheory else return)
            (passes rmb (ren thy)) )
         case l of
           Left  e -> error $ "Failed to prove formula(s):\n  " ++ (intercalate "\n  " e)
           Right m -> do putStrLn "\nSummary:"
                         m
  where
  ren = renameWith (\ x -> [ I i (varStr x) | i <- [0..] ])
  passes rmb =
    head . freshPass
      (runPasses $
        [ RemoveNewtype
        , UncurryTheory
        , RemoveAliases, CollapseEqual
        , SimplifyGently
        ] ++
        [ RemoveBuiltinBool | rmb ])

data I = I Int String
  deriving (Eq,Ord,Show)

instance PrettyVar I where
  varStr (I x s) = s

instance Name I where
  fresh             = refresh (I undefined "x")
  refresh (I _ s)   = do u <- fresh; return (I u s)
  freshNamed s      = refresh (I undefined s)
  getUnique (I u _) = u

data Prover = Prover
  { prover_cmd    :: String -> Double -> (String,[String])
  , prover_ext    :: String
  , prover_pre    :: [StandardPass]
  , prover_post   :: [StandardPass]
  , prover_pretty :: forall a . Name a => Theory a -> Theory a -> (Doc,AxInfo)
  , prover_pipe   :: AxInfo -> ProcessResult -> Result
  }


isUserAsserted :: Formula a -> Bool
isUserAsserted f = case (fm_info f) of
                     UserAsserted _ -> True
                     _              -> False

loop :: (Name a, Show a) => Args -> Prover -> Theory a -> IO (Either [String] (IO ()))
loop args prover thy = go False conjs [] thy{ thy_asserts = assums }
  where
  (conjs,assums) = theoryGoals thy

  go _     []     [] thy = do putStrLn "Finished!"
                              saveTheory args thy
                              return $ Right (return ())
  go False []     q  _  = do let q' = filter isUserAsserted q
                             if (not $ null q')
                               then return $ Left $ map (flip showFormula thy) q'
                               else return $ Right (return ())
  go True  []     q thy = do putStrLn "Reconsidering conjectures..."
                             go False (reverse q) [] thy
  go b     (c:cs) q thy =
    do (str,m_result) <- tryProve args prover c thy
       case m_result of
         Just proof ->
           do let lms = thy_asserts thy
              let n = (length lms)
              g <- go True cs q thy{ thy_asserts = makeProved n c proof:lms }
              case g of
                Right m -> return $
                  Right $ do putStrLn $ pad (show n) 2 ++ ": " ++ rpad str 40 ++
                                     if null (lemmasUsed proof) then ""
                                        else " using " ++ intercalate ", " (map show (lemmasUsed proof))
                             m
                l -> return l
         Nothing -> go b    cs (c:q) thy


makeProved :: Int -> Formula a -> ProofSketch -> Formula a
makeProved i (Formula _ _ tvs b) p = Formula Assert (Lemma i (Just lemmaName) (Just p)) tvs b
  where lemmaName = "lemma-"++show i -- Name it locally first; will probably be translated later

formulaVars :: Formula a -> [Local a]
formulaVars = fst . forallView . fm_body


tryProve :: (Show a, Name a) => Args -> Prover -> Formula a -> Theory a -> IO (String, Maybe ProofSketch)
tryProve args prover fm thy =
  do let (prenex,term) = extractQuantifiedLocals fm thy
     
     let getLemmaName coord | length prenex > coord = Just $ lcl_name (prenex !! coord)
                            | otherwise = Nothing 
     let getLemmaNames coords = catMaybes $ map getLemmaName coords


     putStrLn "Considering:"
     putStrLn $ "  " ++ (ppTerm (toTerm term))
     IO.hFlush IO.stdout

     ind_order <- getIndOrder args fm thy --[[2],[1],[0],[]]
     let isOk order = all (\var -> var < length prenex) order
     let ind_order' = filter isOk ind_order
     let ind_order_pretty = map getLemmaNames ind_order'
     putStrLn $ "induction order from script:"++show ind_order_pretty

     let tree = freshPass (obligations args fm ind_order' (prover_pre prover)) thy

     ptree :: Tree (Promise [Obligation Result]) <- T.traverse (promise args prover) tree

     let timeout'     = round (timeout args * 1000 * 1000) -- microseconds
         processes    = 2

     workers (Just timeout') processes (interleave ptree)

     (errs,res) <- evalTree (any (not . isSuccess) . map ob_content) ptree

     mresult <- case res of
       Obligation (ObInduction coords _ n) _:_
         | sort (map (ind_num . ob_info) res) == [0..n-1]
         , all (isSuccess . ob_content) res
           -> do if null coords
                    then putStrLn $ "Proved without using induction"
                    else putStrLn $ "Proved by induction on " ++ intercalate ", " (getLemmaNames coords)
                 
                 -- try parsing prover output
                 let steps =
                       [ do (pf,lemmas) <- parsePCL ax_list s
                            putStrLn pf
                            return lemmas
                       | Obligation _ (Success (Just (s,ax_list))) <- res
                       ]
                 
                 lemmas <- if null steps
                             then let numLemmas = (length . thy_asserts) thy
                                  in  return [0..numLemmas-1] -- afaik, all lemmas were used
                             else do putStrLn "Proof:"
                                     lemmas <- concat <$> sequence steps
                                     return lemmas

                 let lemmas' = usort lemmas
                     -- Name lemmas locally first; will probably be translated later
                     lemmaNames = map (\l -> "lemma-"++show l) lemmas'
                 -- TODO: not hardcoded provers
                 return $ Just $ ProofSketch lemmaNames coords Structural "z3-4.4.0" "emna-0.1"

         | otherwise
           -> do putStrLn $ "Confusion :("
                 mapM_ print res
                 return Nothing

       _ -> do putStrLn "Failed to prove."
               return Nothing

     -- mapM_ print res

     sequence_
       [ case e of
           Obligation _ (Unknown (ProcessResult err out _)) -> putStrLn (err ++ "\n" ++ out)
           Obligation (ObInduction coords i _) Disproved ->
             do return ()
                -- putStrLn $ unwords (map (lcl_name . (prenex !!)) coords) ++ " " ++ show i ++ " disproved"
           _ -> print e
       | e <-  errs
       ]

     return (ppTerm (toTerm term), if null res then Nothing else mresult)

getIndOrder :: (Show a, Name a) => Args -> Formula a -> Theory a -> IO ([[Int]])
getIndOrder args f thy = do
  --[(_name,_indvars,features,_body)] <- formulasToFeatures [f] thy 
  let (Library fs _ _) = thyToLib thy
  features <- runSchemesConjecture f fs ["ls","la","fs","fa","ala","als","afs","afa"] 3
  let process = (proc "python" ["./scripts/use_classifier.py", show features, dataDir args]) { cwd = Just (workingDir args) }
  out <- readCreateProcess process ""
  return $ case readMaybe out of
    Nothing -> []
    Just xs -> xs

obligations :: Name a => Args -> Formula a -> [[Int]] -> [StandardPass] -> Theory a -> Fresh (Tree (Obligation (Theory a)))
obligations args fm ind_order pre_passes thy0 =
  requireAny <$>
    sequence
      [ do body' <- freshen (fm_body fm)
           trace (show coords) $ pack coords <$>
             runPasses
               (pre_passes ++ [Induction coords])
               (thy0 { thy_asserts = fm{ fm_body = body' } : thy_asserts thy0})
      | coords <- (sort ind_order . combine) [ i | (Local _ (TyCon t _),i) <- formulaVars fm `zip` [0..]
                              , Just DatatypeInfo{} <- [lookupType t scp]
                              ]
      ]
  where
  scp = scope thy0

  combine xs =
    do i <- [0] ++ reverse [1..indvars args]
       us <- replicateM i xs
       guard (nub us == us)
       return us
  pack coords thys =
    requireAll
      [ Leaf (Obligation (ObInduction coords i (length thys)) thy)
      | (thy,i) <- thys `zip` [0..]
      ]
  -- sort the second list based on the order defined by the first list
  sort (x:xs) ys | x `elem` ys = x : sort xs (delete x ys)
                 | otherwise   = sort xs ys
  sort []     ys               = ys


data Obligation a = Obligation
    { ob_info     :: ObInfo
    , ob_content  :: a
    }
  deriving (Show,Functor,Foldable,Traversable)

data ObInfo
  = ObInduction
      { ind_coords  :: [Int]
      , ind_num     :: Int
      , ind_nums    :: Int
      -- , ind_skolems :: [v]
      -- , ind_terms   :: [Expr v]
      }
  deriving (Eq,Ord,Show)

data Result = Success (Maybe (String,AxInfo)) | Disproved | Unknown ProcessResult
  deriving (Eq,Ord,Show)

isUnknown :: Result -> Bool
isUnknown Unknown{} = True
isUnknown _         = False

isSuccess :: Result -> Bool
isSuccess Success{} = True
isSuccess _         = False

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

promise :: Name a => Args -> Prover -> Obligation (Theory a) -> IO (Promise [Obligation Result])
promise params Prover{..} (Obligation info thy) =
  do u <- newUnique
     let filename = "/tmp/" ++ show (hashUnique u) ++ prover_ext
     when (filenames params) (putStrLn filename)
     let (thy_pretty,axiom_list) = prover_pretty thy (head (freshPass (runPasses prover_post) thy))
     writeFile filename (show thy_pretty)
     let (prog,args) = prover_cmd filename (timeout params)
     promise <- processPromise prog args ""

     let update :: PromiseResult ProcessResult -> PromiseResult [Obligation Result]
         update Cancelled = Cancelled
         update Unfinished = Unfinished
         update (An pr) = An [Obligation info (prover_pipe axiom_list pr)]

     return promise{ result = fmap update (result promise) }

showCeil :: Double -> String
showCeil = show . (ceiling :: Double -> Integer)

z3 :: Prover
z3 = Prover
  { prover_cmd = \ filename t -> ("z3",["-smt2",filename,"-t:" ++ showCeil (t*1000)])
  , prover_ext = ".smt2"
  , prover_pre =
      [ TypeSkolemConjecture, Monomorphise False
      , SimplifyGently, LambdaLift, AxiomatizeLambdas, Monomorphise False
      , SimplifyGently, CollapseEqual, RemoveAliases
      , SimplifyGently
      ]
  , prover_post =
      [ AxiomatizeFuncdefs2
      , RemoveMatch -- in case they appear in conjectures
      , SkolemiseConjecture
      , NegateConjecture
      ]
  , prover_pretty = \ _ thy -> (SMT.ppTheory thy,[])
  , prover_pipe =
      \ _ pr@(ProcessResult err out exc) ->
          if "unsat" `isInfixOf` out
             then Success Nothing
             else Unknown pr
  }

waldmeister :: Prover
waldmeister = Prover
  { prover_cmd = \ filename t -> ("waldmeister",filename:["--auto","--output=/dev/stderr","--pcl","--expert","-tl",showCeil t])
  , prover_ext = ".w"
  , prover_pre =
      [ TypeSkolemConjecture, Monomorphise False
      , LambdaLift, AxiomatizeLambdas, LetLift
      , CollapseEqual, RemoveAliases
      , Monomorphise False
      ]
  , prover_post =
      [ AxiomatizeFuncdefs2, AxiomatizeDatadeclsUEQ
      , SkolemiseConjecture
      , UniqLocals
      ]
  , prover_pretty = \ orig -> Waldmeister.ppTheory . niceRename orig
  , prover_pipe =
      \ ax_list pr@(ProcessResult err out exc) ->
          if "Waldmeister states: Goal proved." `isInfixOf` out
             then Success (Just (err,ax_list))
             else if "Waldmeister states: System completed." `isInfixOf` out
               then Disproved
               else Unknown pr
  }
  where

pad :: String -> Int -> String
pad s i = replicate (i - length s) ' ' ++ s

rpad :: String -> Int -> String
rpad s i = s ++ replicate (i - length s) ' '

parsePCL :: AxInfo -> String -> IO (String,[Int])
parsePCL axiom_list s =
  do (exc, out, err) <-
       readProcessWithExitCode
         "proof"
         (words "-nolemmas -nosubst -noplace -nobrackets")
         s

     let axs = mapMaybe unaxiom . lines $ out

     let matches =
           [ (n,i)
           | (n,uv) <- axs
           , (i,st) <- axiom_list
           , matchEq uv st
           ]

     let collected :: ([String],[Info String],[String])
         collected@(_,used,_) = collect matches . drop 2 . dropWhile (/= "Proof:") . lines $ out

     return (unlines (fmt collected),[ i | Lemma i n mp <- used ])
             {-
             ++ "\n" ++ out
             ++ "\n" ++ unlines [ ppTerm e1 ++ " = " ++ ppTerm e2 ++ " " ++ show n | (n,(e1,e2)) <- axs ]
             ++ "\n" ++ unlines [ ppTerm e1 ++ " = " ++ ppTerm e2 ++ " " ++ prettyInfo ax  | (ax,(e1,e2)) <- axiom_list ]
             ++ "\n" ++ "matches: \n" ++
             unlines [ show n ++ " : " ++ prettyInfo i | (n,i) <- matches ]
             ++ "\n" ++ s
             -}

  where
  fmt :: ([String],[Info String],[String]) -> [String]
  fmt (eqs,reasons,thm:_)
    = ( " To show: " ++ ppEquation to_show)
    : [ "     " ++ prettyInfo i ++ ": " ++ ppEquation ih
        ++ if null vars then "" else "  (for all " ++ intercalate ", " vars ++ ")"
      | (i@(IH _),ih) <- axiom_list
      , let vars = nub (nodesOfEq ih) \\ nub (nodesOfEq to_show)
      ] ++
      ""
    : [ h ++ " " ++ u ++ replicate (2 + l - length u) ' ' ++ r
      | (h,(u,r)) <-
          ("  ":repeat " =") `zip`
          (eqs `zip` ("":[ "[" ++ prettyInfo r ++ "]" | r <- reasons ]))
      ]
    where
    Just to_show = readEquation (drop (length "  Theorem 1: ") thm)
    l = maximum (0:map length eqs)

  collect :: [(Int,Info String)] -> [String] -> ([String],[Info String],[String])
  collect ms ((' ':' ':' ':' ':t):ts)
    | Just u <- readTerm t
    , (a,b,c) <- collect ms ts
    = (ppTerm u:a,b,c)
  collect ms ((' ':'=':' ':' ':' ':' ':s):ts)
    | (byax',rest) <- splitAt (length byax) s
    , byax == byax'
    , [(num,blabla)] <- reads rest
    , Just i <- lookup num ms
    , (a,b,c) <- collect ms ts
    = (a,i:b,c)
  collect ms (what:ts)
    | (a,b,c) <- collect ms ts
    = (a,b,what:c)
  collect _ [] = ([],[],[])

  ax = "  Axiom "

  unaxiom s
    | (ax',rest) <- splitAt (length ax) s
    , ax == ax'
    , [(num,':':' ':terms)] <- reads rest
    , Just (t1,t2) <- readEquation terms
    = Just (num :: Int,(t1,t2))
  unaxiom _ = Nothing

  reparse (' ':xs) = ' ':reparse xs
  reparse [] = []
  reparse xs =
    let (u,v) =  span (/= ' ') xs
    in  beautifyTerm u ++ reparse v

  byax = "by Axiom "

prettyInfo :: Info String -> String
prettyInfo i =
  case i of
    Definition f      -> ren f ++ " def"
    IH i              -> "IH" ++ show (i+1)
    Lemma i (Just n) p -> "lemma " ++ show i ++ "(" ++ show n ++ ")"
    Lemma i n p       -> "lemma " ++ show i
    DataDomain d      -> ""
    DataProjection d  -> d ++ " projection"
    DataDistinct d    -> ""
    Defunction f      -> "by defunctionalisation of " ++ f
    UserAsserted (Just i) -> show i
    _                 -> ""


