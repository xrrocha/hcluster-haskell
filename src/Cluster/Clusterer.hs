module Cluster.Clusterer where

import Cluster.Types
import Cluster.SimilarityMatrix
import Cluster.Dendrogram
import Cluster.Util

import Data.Maybe
import Data.Ord(comparing)
import Data.Function(on)
import Data.List
import qualified Data.Vector as V

import Debug.Trace

cluster :: PairGenerator a -> (a -> a -> Similarity) -> [a] -> [Cluster]
cluster g f es = c
    where (c, _, _) = doCluster g f es

doCluster :: PairGenerator a -> (a -> a -> Similarity) -> [a] -> ([Cluster], SimilarityMatrix, Dendrogram)
doCluster g f [] = error "Can't cluster emtpy list"
doCluster g f es = (findBestClusters m d, m, d)
    where  v = V.fromList es
           m = computeMatrix g f v
           d = buildDendrogram (map fromCentroid [0..length es - 1]) m

fromElements :: SimilarityMatrix -> [Cluster] -> Cluster
fromElements m [c1@(Cluster c s d)] = Cluster c s [c1]
fromElements m cs = Cluster c s cs
    where (c, s) = findCentroid m cs

findCentroid :: SimilarityMatrix -> [Cluster] -> (Centroid, Similarity)
findCentroid m =
    head .
    sortBy (descending snd) .
    map (\l -> (fst . head $ l, (sum $ map snd l) / (fromIntegral $ length l))) .
    groupBy ((==) `on` fst) .
    map (\(i, j) -> (i, similarity i j m)) .
    filter (\(i, j) -> i /= j) .
    map ((\(c1@(Cluster i1 _ _), c2@(Cluster i2 _ _)) -> (i1, i2))) .
    selfCartesianProduct

findBestClusters :: SimilarityMatrix -> Dendrogram -> [Cluster]
findBestClusters m d = bestClusters bt ps
    where  ts = findCutThresholds d
           ps = buildThresholdClusterPairs m d ts
           tds = buildThresholdQualityPairs ps
           mq = findMaximumQuality tds
           bt = findBestThreshold tds mq

findBestThreshold :: [(Threshold, Similarity)] -> Similarity -> Threshold
findBestThreshold tds mq = fst $ fromJust $ find (\(_, q) -> q == mq) tds

bestClusters :: Threshold -> [(Threshold, [Cluster])] -> [Cluster]
bestClusters bt [] = []
bestClusters bt ps = snd $ fromJust $ find (\(t, _) -> t == bt) ps

computeMatrix :: PairGenerator a -> (a -> a -> Similarity) -> V.Vector a -> SimilarityMatrix
computeMatrix g f v = buildSimilarityMatrix f' p
    where p = g v
          f' i j = f (v V.! i) (v V.! j)

findCutThresholds dendrogram = sort $ nub $ go [] dendrogram
    where go acc (Leaf i)       = acc
          go acc (Branch s l r) = go (go (s : acc) l) r

buildThresholdClusterPairs :: SimilarityMatrix -> Dendrogram -> [Threshold] -> [(Threshold, [Cluster])]
buildThresholdClusterPairs m d ts = map buildPair ts
    where buildPair t = (t, clustersForThreshold m d t)

clustersForThreshold :: SimilarityMatrix -> Dendrogram -> Threshold -> [Cluster]
clustersForThreshold m d t = map (fromElements m) cs
    where  cs = map elements ds
           ds = cutAt d t

buildThresholdQualityPairs :: [(Threshold, [Cluster])] -> [(Threshold, Similarity)]
buildThresholdQualityPairs ps = map (\(t, cs) -> (t, clusterQuality cs)) ps

clusterQuality :: [Cluster] -> Similarity
clusterQuality cs = sum $ map clusterSimilarity cs
    where clusterSimilarity (Cluster _ s _) = s

findMaximumQuality :: [(Threshold, Similarity)] -> Similarity
findMaximumQuality tcs = maximum $ map (\(t, s) -> s) tcs

fromCentroid :: Centroid -> Cluster
fromCentroid c = Cluster c 0.0 []

clusterElements :: Cluster -> [Int]
clusterElements c = go [] c
    where go acc (Cluster c _ []) = c : acc
          go acc (Cluster _ _ a) = acc ++ foldl go [] a
