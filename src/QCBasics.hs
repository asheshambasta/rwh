module QCBasics where

import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
               where lhs = filter (<x) xs
                     rhs = filter (>=x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum xs = not (null xs) ==> minimum xs == head (qsort xs)

prop_ordered xs = ordered (qsort xs)
                where ordered []       = True
                      ordered [x]      = True
                      ordered (x:y:xs) = x <= y && ordered xs
prop_permutation xs  = permutation xs (qsort xs)
                    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_lastmax xs = not (null xs) ==>
                    maximum sorted == last sorted
                    where sorted = qsort xs

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
      minimum sorted == min (minimum xs) (minimum ys)
      where sorted = qsort (xs ++ ys)

prop_sort_model xs = sort xs == qsort xs
