module Cluster.Dendrogram where

import Cluster.Types
import Cluster.SimilarityMatrix
import Cluster.Util

import Data.IntMap
import Data.List(sortBy)

data Dendrogram =
      Leaf Cluster
    | Branch Similarity Dendrogram Dendrogram
    deriving (Show)

elements :: Dendrogram -> [Cluster]
elements = go []
    where go acc (Leaf i)       = i : acc
          go acc (Branch _ l r) = go (go acc r) l

cutAt :: Dendrogram -> Similarity -> [Dendrogram]
cutAt dendrogram threshold = go [] dendrogram
    where go acc x@(Leaf _)                        = x : acc
          go acc x@(Branch d l r) | d >= threshold = x : acc
                                  | otherwise      = go (go acc r) l

buildDendrogram :: [Cluster] -> SimilarityMatrix -> Dendrogram
buildDendrogram cs m = d
    where (_, d) = findMax $ agglomerate m l
          l = fromList $ zip [i | i <- [0..length cs -1]] $ Prelude.map Leaf cs--[(i, Leaf $ fromCentroid i) | i <- [0..s - 1]]

agglomerate :: SimilarityMatrix -> IntMap Dendrogram-> IntMap Dendrogram
agglomerate m ds =
  if size ds' == 1 then ds'
  else agglomerate m ds'
  where ds'= merge m ds

merge :: SimilarityMatrix -> IntMap Dendrogram-> IntMap Dendrogram
merge m ds = Prelude.foldl merge' ds ods
    where merge' ds (i, j, d) =
            if not ((member i ds) && (member j ds)) then
                ds
            else
                let  nk = (fst $ findMax ds) + 1
                     nb = Branch d left right
                     left = ds ! i
                     right = ds ! j
                in   delete i $
                     delete j $
                     insert nk nb ds
          ods = sortBy (descending thrd) ss
          ss = Prelude.map computeSimilarity ps
          ps = [(i, j) | i <- keys ds, j <- keys $ snd $ split i ds]
          computeSimilarity (i, j) = (i, j, dendrogramSimilarity m l r)
            where l  = ds ! i
                  r = ds ! j

dendrogramSimilarity ::  SimilarityMatrix -> Dendrogram -> Dendrogram -> Similarity
dendrogramSimilarity m l r =
    if length ds == 0 then 0.0
    else (sum ds) / (fromIntegral $ length ds)
    where ds = Prelude.map (\(c1@(Cluster i1 _ _), c2@(Cluster i2 _ _)) -> similarity i1 i2 m) ps
          ps = cartesianProduct (elements l) (elements r)
