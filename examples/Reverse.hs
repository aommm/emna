module Reverse where

import Tip
import qualified Prelude

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- lemma_assoc xs ys zs = xs ++ (ys ++ zs) === (xs ++ ys) ++ zs
--
-- lemma_rid xs = xs ++ [] === xs
--
-- lemma xs ys = reverse xs ++ reverse ys === reverse (ys ++ xs)

conj xs = reverse (reverse xs) === xs

