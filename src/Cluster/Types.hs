module Cluster.Types where

import qualified Data.Vector as V

type Similarity = Double
type Threshold = Similarity
type PairGenerator a = V.Vector a -> [(Int, Int)]

type Centroid = Int
data Cluster = Cluster Centroid Similarity [Cluster]
               deriving(Show)
