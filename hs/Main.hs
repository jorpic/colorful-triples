
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.List (sort)

-- import View (viewRoot)
-- import GraphAlgos (linkMap, connectedComponents)
import Triple as T
import Utils (info, timeIt)



_MAX :: Int
_MAX = 7825

py3 :: Triples
py3 = T.pyth _MAX


main :: IO ()
main = do
  timeIt $ info "number of triples: %s" $ Set.size py3

  py3' <- timeIt $ do
    let py3' = dropPendants py3
    info "number of triples without pendants: %s" $ Set.size py3'
    return py3'

  let links = linksMap py3'

  mapM_ print
    $ sort
    [ (t, Set.size ws)
      | x <- Set.toList py3'
      , let t = Triple x
      , let ws = wheels t links
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


pendants :: Triples -> Triples
pendants
  = Set.fromList . map asInt . concat
  . Map.elems . Map.filter ((== 1) . length)
  . linksMap

dropPendants :: Triples -> Triples
dropPendants triples = case pendants triples of
  ps | Set.null ps -> triples
     | otherwise   -> dropPendants $ triples Set.\\ ps


wheels :: Triple -> Links -> Triples
wheels root ls = Set.fromList $ map asInt $ concat $ Map.elems frontLinks
  where
    visitedTriples = Set.fromList [asInt root]
    visitedLinks = Set.fromList $ concatMap links [root]
    frontTriples = Set.fromList
      [ asInt t
      | l <- links root
      , t <- ls Map.! l
      , Set.notMember (asInt t) visitedTriples
      ]
    frontLinks
      = Map.filterWithKey
        (\l xs -> Set.notMember l visitedLinks && length xs > 1)
      $ linksMap frontTriples

-- agg :: Foldable t => (a, a -> a -> a) -> t Map.Key -> IntMap a
-- agg (a,f) = foldl' (\m x -> Map.insertWith f x a m) Map.empty
