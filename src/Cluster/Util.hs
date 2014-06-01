module Cluster.Util where

import qualified Data.Vector as V

selfCartesianProduct :: [a] -> [(a, a)]
selfCartesianProduct a = cartesianProduct a a

cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

vectorPermutations :: V.Vector a -> [(Int, Int)]
vectorPermutations v = [(i, j) | i <- [0..end], j <- [i + 1..end]]
    where end = V.length v - 1

thrd :: (a, b, c) -> c
thrd (_, _, v) = v

descending :: Ord b => (a -> b) -> a -> a -> Ordering
descending f a1 a2 = descending' (f a1) (f a2)
    where descending' b1 b2
            | b1 < b2 = GT
            | b1 > b2 = LT
            | b1 == b2 = EQ
