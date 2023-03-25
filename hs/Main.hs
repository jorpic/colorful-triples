
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map


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

  timeIt $ do
    let py3' = dropPendants py3
    info "number of triples without pendants: %s" $ Set.size py3'

  -- After dropping pendants all the triples belong to a single component.
  -- This component is "dense" in a sense that dropping any of links does not
  -- break it into subcomponents.


-- TODO:
--  - reconstruct full graph from primitive triples
--  - distribution of point weights (=> a lot of points has small weight)
--  - viewRoots


pendants :: Triples -> Triples
pendants
  = Set.fromList . map asInt . concat
  . Map.elems . Map.filter ((== 1) . length)
  . linksMap

dropPendants :: Triples -> Triples
dropPendants triples = case pendants triples of
  ps | Set.null ps -> triples
     | otherwise   -> dropPendants $ triples Set.\\ ps


-- agg :: Foldable t => (a, a -> a -> a) -> t Map.Key -> IntMap a
-- agg (a,f) = foldl' (\m x -> Map.insertWith f x a m) Map.empty
