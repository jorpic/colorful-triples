
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
    [ (t, Set.size ws)
      | t <- T.graphTriplesList py3
      , let ws = wheels t py3
      , not $ Set.null ws
    ]

  -- After dropping pendants all the triples belong to a single component.
  -- This component is "dense" in a sense that dropping any of links does not
  -- break it into subcomponents.


-- TODO:
--  - reconstruct full graph from primitive triples
--  - distribution of point weights (=> a lot of points has small weight)
--  - viewRoots
--


pendants :: Graph -> Triples
pendants
  = T.triplesFromList . concat
  . Map.elems . Map.filter ((== 1) . length)
  . T.graphLinks

dropPendants :: Graph -> Graph
dropPendants g = case pendants g of
  ps | Set.null ps -> g
     | otherwise   -> dropPendants $ T.mkGraph $ T.graphTriples g Set.\\ ps


wheels :: Triple -> Graph -> Triples
wheels root g = T.triplesFromList $ concat $ Map.elems frontLinks
  where
    visitedTriples = Set.fromList [T.asInt root]
    visitedLinks = Set.fromList $ concatMap T.links [root]

    frontTriples = T.triplesFromList
      [ t
      | l <- T.links root
      , t <- T.graphLinks g Map.! l
      , Set.notMember (T.asInt t) visitedTriples
      ]
    frontLinks
      = Map.filterWithKey
        (\l xs -> Set.notMember l visitedLinks && length xs > 1)
      $ T.mkLinks frontTriples
