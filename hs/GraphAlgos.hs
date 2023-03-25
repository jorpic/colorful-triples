module GraphAlgos where

import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.List (foldl', nub)

type Triple = [Int]

linkMap :: [Triple] -> IntMap [Triple]
linkMap = foldl'
  (\m t -> foldl' (\n k -> Map.insertWith (++) k [t] n) m t)
  Map.empty


-- FIXME: prevent looping
neighbors :: Int -> Int -> Int -> [Triple] -> [Triple]
neighbors depth width root xs
  | depth <= 0 = []
  | otherwise = nub $ layer0 ++ layer1
  where
    layer0 = filter (elem root) xs
    layer1 = concat
      $ map (\r -> take width $ neighbors (depth-1) width r xs)
      $ nub $ concat layer0
