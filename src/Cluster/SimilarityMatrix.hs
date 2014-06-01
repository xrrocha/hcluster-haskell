module Cluster.SimilarityMatrix where

import Cluster.Types

import Data.IntMap

data SimilarityMatrix = SimilarityMatrix (IntMap (IntMap Similarity))
              deriving (Eq, Show)

buildSimilarityMatrix :: (Int -> Int -> Similarity) -> [(Int, Int)] -> SimilarityMatrix
buildSimilarityMatrix f p = SimilarityMatrix m
    where m = Prelude.foldl add empty p
          add m (i, j) = if s == 0.0 then m else alter add' l m
                where s = f l r
                      l = min i j
                      r = max i j
                      add' m = Just $
                        case m of Nothing -> singleton r s
                                  Just dm -> insert    r s dm

similarity :: Int -> Int -> SimilarityMatrix -> Similarity
similarity i j (SimilarityMatrix m) = findWithDefault 0.0 r $ findWithDefault empty l m
    where l = min i j
          r = max i j
