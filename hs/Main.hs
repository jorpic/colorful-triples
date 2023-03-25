
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.List (sort)

import Triple (Triples, Triple, Graph)
import Triple qualified as T
import Utils (info, timeIt)


main :: IO ()
main = do
  py3 <- timeIt $ do
    let g0 = T.pyth 7825
    info "number of triples: %s" $ Set.size $ T.graphTriples g0
    let g1 = dropPendants g0
    info "number of triples without pendants: %s"
      $ Set.size $ T.graphTriples g1
    return g1

  mapM_ print
    $ sort
    [ (map Set.size ws, t)
      | t <- T.graphTriplesList py3
      , let ws = wheel t py3
      , not $ null ws
    ]

  -- mapM_ print
  --   $ sort $ map (map T.Triple . Set.toList)
  --   $ wheel (T.mkTriple 4695 6260 7825) py3

  -- After dropping pendants all the triples belong to a single component.
  -- This component is "dense" in a sense that dropping any of links does not
  -- break it into subcomponents.


-- TODO:
--  - reconstruct full graph from primitive triples
--  - distribution of point weights (=> a lot of points has small weight)
--  - viewRoots


pendants :: Graph -> Triples
pendants
  = T.triplesFromList . concat
  . Map.elems . Map.filter ((== 1) . length)
  . T.graphLinks

dropPendants :: Graph -> Graph
dropPendants g = case pendants g of
  ps | Set.null ps -> g
     | otherwise   -> dropPendants $ T.mkGraph $ T.graphTriples g Set.\\ ps


wheel :: Triple -> Graph -> [Triples]
wheel root g = loop
  (T.triplesFromList [root])
  [root]
  where
    loop visitedTriples roots
      | Set.null newRoots = []
      | otherwise
        = newRoots : loop visitedTriples' (map T.Triple $ Set.toList newRoots)
      where
        rootLinks = Set.fromList $ concatMap T.links roots
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
