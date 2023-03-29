
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.List (foldl', sort, nub)

import System.Environment (getArgs)

import Algos (dropPendants, wheel)
import View qualified as V
import Triple qualified as T
import Utils (info)

main :: IO ()
main = do
  py3 <- do
    let g0 = T.pyth 7825
    info "number of triples: %s" $ Set.size $ T.graphTriples g0
    let g1 = dropPendants g0
    info "number of triples without pendants: %s"
      $ Set.size $ T.graphTriples g1
    info "number of links: %s"
      $ Map.size $ T.graphLinks g1
    return g1

  let linkWeightsCount
        = Map.toList
        $ foldl' (\m ts -> Map.insertWith (+) (length ts) 1 m) Map.empty
        $ Map.elems $ T.graphLinks py3

  putStrLn "most of the links connect only small number of triples:"
  print $ take 5 linkWeightsCount

  -- After dropping pendants all the triples belong to a single component.
  -- This component is "dense" in a sense that dropping any of links does not
  -- break it into subcomponents.

  -- There are a lot of links connecting only two or three nodes.

  -- Lets see how the closest neighbourhood of these links looks like.


  -- print all wheels ordered by link weight
  -- mapM_ print
  --   $ sort
  --   [ (ws, l)
  --   | (l, ts) <- Map.toList $ T.graphLinks py3
  --   , let ws = map Set.size $ wheel ts py3
  --   , length ws > 1
  --   ]

  l <- head <$> getArgs
  V.viewWheels l
    $ wheel (T.graphLinks py3 Map.! read l) py3
