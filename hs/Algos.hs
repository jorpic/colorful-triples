module Algos where

import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map

import Triple (Triples, Triple(..), Graph)
import Triple qualified as T

pendants :: Graph -> Triples
pendants
  = T.triplesFromList . concat
  . Map.elems . Map.filter ((== 1) . length)
  . T.graphLinks

dropPendants :: Graph -> Graph
dropPendants g = case pendants g of
  ps | Set.null ps -> g
     | otherwise   -> dropPendants $ T.mkGraph $ T.graphTriples g Set.\\ ps


wheels :: Int -> [Triple] -> Graph -> [Triples]
wheels maxLinks roots g = rootSet : loop rootSet roots
  where
    rootSet = T.triplesFromList roots

    loop visitedTriples rs
      | Set.null newRoots = []
      | Set.size (allLinks visitedTriples') > maxLinks = []
      | otherwise
        = newRoots : loop visitedTriples' (map T.Triple $ Set.toList newRoots)
      where
        rootLinks = Set.fromList $ concatMap T.links rs
        -- Get triples connected to the root but exclude already visited
        -- ones (excluding just triples from prev layer should be enough?).
        -- We call those frontTriples because they are on the front
        -- edge of our BFS expansion.
        frontTriples = T.triplesFromList
          [ t
          | l <- Set.toList rootLinks
          , t <- T.graphLinks g Map.! l
          , Set.notMember (T.asInt t) visitedTriples
          ]

        -- Next we extract links between those frontTriples. Then:
        --  - drop links to the inner layer (the ones that we just
        --  traversed to get tot frontTriples from roots)
        --  - drop links that lead only to the next layer/front.
        -- Remaining links connect some triples from frontTriples with each
        -- other.
        frontLinks
          = Map.filterWithKey
            (\l xs -> Set.notMember l rootLinks && length xs > 1)
          $ T.mkLinks frontTriples

        -- Extract those triples that are connected with each other.
        newRoots
          = T.triplesFromList $ concat $ Map.elems frontLinks

        visitedTriples' = Set.union visitedTriples newRoots


allLinks :: Triples -> Set.IntSet
allLinks = Set.fromList . concatMap (T.links . Triple) . Set.elems
