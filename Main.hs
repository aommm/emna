{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Tip.HaskellFrontend
import Tip.QuickSpec
import Tip.Core hiding (Unknown)
import Tip.Fresh
import Tip.Passes
import Tip.Pretty
import Tip.Scope
import Tip.Pretty.SMT as SMT

import Debug.Trace

import Tip.Utils.Rename
import Tip.Utils (usort)

import Data.Generics.Geniplate

import Data.Maybe
import Data.List (sort, sortBy, isInfixOf, nub, (\\), intercalate)
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

import System.Environment

import Waldmeister

import qualified System.IO as IO
import System.Console.CmdArgs
import System.Process (readProcessWithExitCode)
import System.Directory (makeAbsolute)

import qualified Data.Map as M
import Data.Map (Map)

data Args =
  Args
    { file    :: String
    , explore :: Bool
    , indvars :: Int
    , timeout :: Double
    , filenames :: Bool
    , prover    :: String
    , output    :: Maybe String
    }
  deriving (Show,Data,Typeable)

defArgs :: Args
defArgs =
  Args
    { file    = ""    &= argPos 0 &= typFile
    , explore = True  &= name "e" &= help "Explore theory"
    , indvars = 1     &= name "v" &= help "Number of variables to do induction on"
    , timeout = 1     &= name "t" &= help "Timeout in seconds (default 1)"
    , filenames = False           &= help "Print out filenames of theories"
    , prover = "w"                &= help "Prover (waldmeister/z3)"
    , output = Nothing &= name "o" &= help "Proof output file"
    }
  &= program "emna" &= summary "simple inductive prover with proof output"

-- Parse command line arguments
parseArgs :: IO Args
parseArgs = do
  args <- cmdArgs defArgs
  if isJust (output args)
    then do
      -- dereference hej to path/to/cwd/hej
      -- TODO: doesn't defererence ~, . or ..
      let (Just path) = output args
      path' <- makeAbsolute path
      return args {output = Just path'}
    else do
      return args

main :: IO ()
main = do
  args@Args{..} <- parseArgs
  x <- readHaskellOrTipFile file defaultParams
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

isUserAsserted :: Formula a -> Bool
isUserAsserted f = case (fm_info f) of
                     UserAsserted -> True
                     _            -> False

showFormula :: Name a => Formula a -> Theory a -> String
showFormula fm thy = ppTerm $ toTerm $ snd $ extractQuantifiedLocals fm thy

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
         Just proof@(lemmas,coords) ->
           do let lms = thy_asserts thy
              let n = (length lms)
              g <- go True cs q thy{ thy_asserts = makeProved n c proof:lms }
              case g of
                Right m -> return $
                  Right $ do putStrLn $ pad (show n) 2 ++ ": " ++ rpad str 40 ++
                                     if null lemmas then ""
                                        else " using " ++ intercalate ", " (map show lemmas)
                             m
                l -> return l
         Nothing -> go b    cs (c:q) thy

makeProved :: Int -> Formula a -> ProofSketch -> Formula a
makeProved i (Formula _ _ tvs b) p = Formula Assert (Lemma i (Just p)) tvs b

formulaVars :: Formula a -> [Local a]
formulaVars = fst . forallView . fm_body

extractQuantifiedLocals :: Name a => Formula a -> Theory a -> ([Local String], Expr String)
extractQuantifiedLocals fm thy =
             forallView $ fm_body $ head $ thy_asserts $ fmap (\ (Ren x) -> x)
             $ niceRename thy thy{thy_asserts = [fm], thy_funcs=[]}

tryProve :: Name a => Args -> Prover -> Formula a -> Theory a -> IO (String, Maybe ProofSketch)
tryProve args prover fm thy =
  do let (prenex,term) = extractQuantifiedLocals fm thy

     putStrLn "Considering:"
     putStrLn $ "  " ++ (ppTerm (toTerm term))
     IO.hFlush IO.stdout

     let tree = freshPass (obligations args fm (prover_pre prover)) thy

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
                    else putStrLn $ "Proved by induction on " ++ intercalate ", " (map (lcl_name . (prenex !!)) coords)
                 
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
                 return $ Just (lemmas', coords)

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

obligations :: Name a => Args -> Formula a -> [StandardPass] -> Theory a -> Fresh (Tree (Obligation (Theory a)))
obligations args fm pre_passes thy0 =
  requireAny <$>
    sequence
      [ do body' <- freshen (fm_body fm)
           pack coords <$>
             runPasses
               (pre_passes ++ [Induction coords])
               (thy0 { thy_asserts = fm{ fm_body = body' } : thy_asserts thy0})
      | coords <- combine [ i | (Local _ (TyCon t _),i) <- formulaVars fm `zip` [0..]
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

-- Save theory to file
saveTheory :: (Show a) => Args -> Theory a -> IO ()
saveTheory args thy = do
  when (hasOutput args) $ do
    -- TODO: only absolute path works here, why!?
    -- http://stackoverflow.com/questions/21765570/haskell-compilation-with-an-input-file-error-openfile-does-not-exist-no-such
    let (Just file) = output args
    -- format theory as string
    let thyString   = show thy
    putStrLn $ "saving theory to "++ file ++ "..."
    writeFile file (thyString)
    putStrLn "... done!"
    return ()
  where hasOutput = isJust . output

data Prover = Prover
  { prover_cmd    :: String -> (String,[String])
  , prover_ext    :: String
  , prover_pre    :: [StandardPass]
  , prover_post   :: [StandardPass]
  , prover_pretty :: forall a . Name a => Theory a -> Theory a -> (Doc,AxInfo)
  , prover_pipe   :: AxInfo -> ProcessResult -> Result
  }

promise :: Name a => Args -> Prover -> Obligation (Theory a) -> IO (Promise [Obligation Result])
promise args Prover{..} (Obligation info thy) =
  do u <- newUnique
     let filename = "/tmp/" ++ show (hashUnique u) ++ prover_ext
     when (filenames args) (putStrLn filename)
     let (thy_pretty,axiom_list) = prover_pretty thy (head (freshPass (runPasses prover_post) thy))
     writeFile filename (show thy_pretty)
     let (prog,args) = prover_cmd filename
     promise <- processPromise prog args ""

     let update :: PromiseResult ProcessResult -> PromiseResult [Obligation Result]
         update Cancelled = Cancelled
         update Unfinished = Unfinished
         update (An pr) = An [Obligation info (prover_pipe axiom_list pr)]

     return promise{ result = fmap update (result promise) }

z3 :: Prover
z3 = Prover
  { prover_cmd = \ filename -> ("z3",["-smt2",filename])
  , prover_ext = ".smt2"
  , prover_pre =
      [ TypeSkolemConjecture, Monomorphise False
      , SimplifyGently, LambdaLift, AxiomatizeLambdas, Monomorphise False
      , SimplifyGently, CollapseEqual, RemoveAliases
      , SimplifyGently
      ]
  , prover_post =
      [ AxiomatizeFuncdefs2
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
  { prover_cmd = \ filename -> ("waldmeister",filename:["--auto","--output=/dev/stderr","--pcl"])
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

     return (unlines (fmt collected),[ i | Lemma i mp <- used ])
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
    Lemma i p         -> "lemma " ++ show i
    DataDomain d      -> ""
    DataProjection d  -> d ++ " projection"
    DataDistinct d    -> ""
    Defunction f      -> "by defunctionalisation of " ++ f
    UserAsserted      -> ""
    _                 -> ""

newtype Ren = Ren String
  deriving (Eq,Ord,Show)

instance PrettyVar Ren where
  varStr (Ren s) = case s of
                     "x" -> "v"
                     _   -> s

data Prod f g a = [a] :*: g a
  deriving (Eq,Ord,Show,Functor,Traversable,Foldable)

sND (_ :*: b) = b

niceRename :: (Ord a,PrettyVar a) => Theory a -> Theory a -> Theory Ren
niceRename thy_orig thy =
  sND $
  fmap Ren $
  renameWith (disambig $ suggestWithTypes lcl_rn gbl_rn thy_orig thy)
             (concat interesting :*: thy)
  where
  interesting =
    sortBy (flip $ comparing length)
      [ [ k2 `asTypeOf` k
        | Gbl (Global k2 (PolyType _ [] _) _) :@: _ <- as
        , varStr k2 `notElem` constructors
        ]
      | Formula Prove _ _ fm <- thy_asserts thy
      , Gbl (Global k _ _) :@: as <- universeBi fm
      , varStr k `elem` constructors
      ]

  lcl_rn (Local x t)
    | is "tree" t = "p"
    | is "list" t = "xs"
    | is "nat" t  = "n"
    | is "bool" t = "p"
    | otherwise   = "x"

  is s t =
    case fmap varStr t of
      TyCon list _ -> s `isInfixOf` map toLower list
      _ -> False

  constructors =
    [ varStr k
    | (k,ConstructorInfo{}) <- M.toList (Tip.Scope.globals (scope thy_orig))
    ]

  gbl_rn a _ | varStr a `elem` constructors = varStr a
  gbl_rn a (FunctionInfo (PolyType _ [] t))
    | not (any ((>= 2) . length) interesting)
      || or [ a `elem` xs | xs <- interesting, length xs >= 2 ]
       = case () of
           () | is "list" t -> "as" -- check that a is element in a list with >=2 elements
              | otherwise   -> "a"  -- in the interesting list
    | otherwise = lcl_rn (Local a t)
  gbl_rn a _ = varStr a

suggestWithTypes ::
  (Ord a, PrettyVar a) =>
  (Local a -> String) ->
  (a -> GlobalInfo a -> String) ->
  Theory a ->
  Theory a ->
  (a -> String)
suggestWithTypes lcl_rn gbl_rn thy_orig thy =
  \ a ->
   case M.lookup a all_locals_orig of
     Just t -> lcl_rn (Local a t)
     Nothing ->
       case lookupGlobal a scp_orig of
         Just gi -> gbl_rn a gi
         Nothing ->
           case M.lookup a all_locals of
             Just t -> lcl_rn (Local a t)
             Nothing ->
               case lookupGlobal a scp of
                 Just gi -> gbl_rn a gi
                 Nothing -> varStr a
  where
  all_locals_orig = M.fromList [ (x,t) | Local x t <- universeBi thy_orig ]
  scp_orig        = scope thy_orig
  all_locals      = M.fromList [ (x,t) | Local x t <- universeBi thy ]
  scp             = scope thy

