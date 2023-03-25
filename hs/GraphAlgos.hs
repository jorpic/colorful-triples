module GraphAlgos where

import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.List (foldl', nub)
import Types


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


connectedComponents :: [Triple] -> [IntSet]
connectedComponents xs@((x:_):_)
  = comp : if length rest == 0
    then []
    else connectedComponents rest
  where
    g = linkMap xs
    loop s a
      | Set.member a s = s
      | otherwise = foldl' loop (Set.insert a s) $ nub $ concat $ g Map.! a
    comp = loop Set.empty x
    rest = filter (any (`Set.notMember` comp)) xs
