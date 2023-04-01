
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.List (foldl', sort, nub)

import System.Environment (getArgs)

import Algos (dropPendants, wheels, allLinks)
import View qualified as V
import Triple qualified as T
import System qualified as S
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

  let smallWheels =
        [ ws
        | (l, ts) <- Map.toList $ T.graphLinks py3
        , let ws = wheels 25 ts py3
        , length ws > 2
        ]
  info "number of small wheels: %s" $ length smallWheels

  let smallWheelsTriples = Set.unions $ map Set.unions smallWheels
  info "number of triples in small wheels: %s" $ Set.size smallWheelsTriples

  -- number of systems close to 7825
  let neighbours = Set.fromList $ map T.asInt $ T.graphLinks py3 Map.! 7825
  info "number of 7825 neighbours: %s" $ Set.size neighbours
  let connectedWheels =
        [ (ws, l)
        | (l, ts) <- Map.toList $ T.graphLinks py3
        , let ws = wheels 27 ts py3
        , length ws > 2
        , let ts' = Set.unions ws
        , not $ Set.disjoint ts' neighbours
        ]

  info "number of connected wheels: %s" $ length connectedWheels
  -- mapM_ print connectedWheels

  -- mapM_ print
  --   [ (a, b, Set.intersection
  --       (Set.unions $ map allLinks wa)
  --       (Set.unions $ map allLinks wb)
  --     )
  --   | (wa, a) <- connectedWheels
  --   , (wb, b) <- connectedWheels
  --   , a > b
  --   ]


  -- let a = 2616
  -- let b = 2817
  -- let wa = wheels 27 (T.graphLinks py3 Map.! a) py3
  -- let sa = S.wheelsToSystem wa
  -- let wb = wheels 27 (T.graphLinks py3 Map.! b) py3
  -- let sb = S.wheelsToSystem wb
  -- print (sa, sb)
  -- print $ S.join sa sb

  l <- head <$> getArgs
  let ws = wheels 164 (T.graphLinks py3 Map.! read l) py3
  mapM_ (print . map T.Triple . Set.toList) ws
  let ws' = map (T.triplesFromList . concat . filter ((>4).length) . Map.elems . T.mkLinks) ws
  V.viewWheels l ws'
  print $ map (Map.size . T.mkLinks) ws
  print $ map (Map.size . T.mkLinks) ws'
  print $ S.wheelsToSystem ws'
