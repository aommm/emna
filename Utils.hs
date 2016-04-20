{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
--{-# LANGUAGE DeriveDataTypeable #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE Rank2Types #-}

module Utils where

import Tip.Core hiding (Unknown)
import Tip.Library
import Tip.Fresh
import Tip.Pretty
import Tip.Scope

import Debug.Trace

import Tip.Utils.Rename
import Tip.Utils (usort)

import Data.Generics.Geniplate

import Data.List (sort, sortBy, isInfixOf, nub, (\\), intercalate, delete)
import Data.Ord
import Data.Char

import Waldmeister

import qualified Data.Map as M
import Data.Map (Map)

newtype Ren = Ren String
  deriving (Eq,Ord,Show)

instance PrettyVar Ren where
  varStr (Ren s) = case s of
                     "x" -> "v"
                     _   -> s

data Prod f g a = [a] :*: g a
  deriving (Eq,Ord,Show,Functor,Traversable,Foldable)

showFormula :: Name a => Formula a -> Theory a -> String
showFormula fm thy = ppTerm $ toTerm $ snd $ extractQuantifiedLocals fm thy

extractQuantifiedLocals :: Name a => Formula a -> Theory a -> ([Local String], Expr String)
extractQuantifiedLocals fm thy =
             forallView $ fm_body $ head $ thy_asserts $ fmap (\ (Ren x) -> x)
             $ niceRename thy thy{thy_asserts = [fm], thy_funcs=[]}

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
