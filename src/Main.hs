module Main where

import Cluster.Types
import Cluster.SimilarityMatrix
import Cluster.Dendrogram
import Cluster.Clusterer
import Cluster.Util

import StringDistance

import Data.List
import Data.Maybe
import Data.Foldable(forM_)
import Text.Printf(printf)
import System.TimeIt(timeItT)
import Control.Exception(evaluate)
import System.IO

import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import Graphics.Gnuplot.Plot
import Control.Concurrent
import Control.Concurrent

main = do
    let filename = "data/spanish-surnames.tsv" --names.tsv
    content <- readFile filename
    let skip = 0
    let load = 100 --maxBound :: Int
    let items =  take load $ drop skip $lines content

    let distanceScorer = xjaro

    (clusteringTime, triplet) <- timeItT $ evaluate $ doCluster vectorPermutations distanceScorer items
    let (clusters, matrix, dendrogram) = triplet
    printf "Created %d clusters in %.4f seconds\n" (length clusters) clusteringTime

    --let finalClusters = Main.agglomerate matrix clusters
    --printf "Agglomerated %d clusters\n" (length finalClusters)

    forM_ clusters $ \cluster@(Cluster centroid similarity elems) ->
        printf "Centroid: %s (%.4f): %d -> %s\n" (items !! centroid) similarity (length elems) (show $ map (\i -> items !! i) $ clusterElements cluster)

    --forM_ clusters $ \cluster@(Cluster centroid similarity elems) ->
    --    printf "Centroid: %s (%.4f): %d -> %s\n" (items !! centroid) similarity (length elems) (show $ map (\i -> items !! i) $ clusterElements cluster)

    printf "Clustered elements: %d\n" (foldl (+) 0 $ map (\c@(Cluster _ _ a) -> length a) clusters)

    putStrLn "Done!"
    -- result <- plotLists [] $ [points]
    --threadDelay 20000

agglomerate :: SimilarityMatrix -> [Cluster] -> [Cluster]
agglomerate _ [c] = [c]
agglomerate m cs =
    if length ncs == length cs then cs
    else Main.agglomerate m  ncs
    where ncs = cluster vectorPermutations ds cs
          ds = \c1@(Cluster i1 _ _) c2@(Cluster i2 _ _) -> similarity i1 i2 m
