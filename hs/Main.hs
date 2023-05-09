
import Data.IntSet qualified as Set
import Data.IntMap.Strict qualified as Map
import Data.List (foldl', sort, nub)

import System.Environment (getArgs)

import Algos (dropPendants, wheels, allLinks)
import View qualified as V
import Triple (Triples, Triple(..))
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


  let root
        = reduceWheels 2
        $ wheels 100 (T.graphLinks py3 Map.! 7825) py3

  let l2 = T.mkLinks $ root !! 1
  let l2Wheels =
        [ (l, ws')
        | (l, ts) <- Map.toList l2
        , let ws = wheels 128 ts py3
        , all ((>2) . Set.size) ws
        , let ws' = if l == 6260
                then [ws !! 0, ws !! 1, reduceWheel 5 $ ws !! 2]
                else ws
        ]

  let allWheels = (7825, root) : l2Wheels
  print $ sort $ nub $ concatMap (T.links . Triple) $ Set.toList $ head root
  mapM_ print $ map (map Triple . Set.toList) root

  mapM_
    (\(l,w) -> V.viewWheels (show l) w)
    allWheels

  -- mapM_
  --   (\(l, w) -> print (l, S.wheelsToSystem $ take 2 w))
  --   allWheels


  -- let (_,a) = allWheels !! 1
  -- S.showSolutions $ S.wheelsToSystem $ take 2 a

  -- mapM_ print
  --   [ (a, b
  --     , Set.size $ Set.intersection (wheelLinks wa) (wheelLinks wb)
  --     , sa, sb
  --     , S.join sa sb
  --     )
  --   | (a, wa) <- allWheels
  --   , (b, wb) <- allWheels
  --   , a < b
  --   , let sa  = S.wheelsToSystem $ take 2 wa
  --   , let sb  = S.wheelsToSystem $ take 2 wb
  --   ]


  -- l <- head <$> getArgs
  -- let ws = wheels 164 (T.graphLinks py3 Map.! read l) py3
  -- mapM_ (print . map Triple . Set.toList) ws
  -- let ws' = reduceWheel 4 ws
  -- V.viewWheels l ws'
  -- print $ map (Map.size . T.mkLinks) ws
  -- print $ map (Map.size . T.mkLinks) ws'
  -- print $ S.wheelsToSystem ws'


countLinks :: Triples -> Int
countLinks = Map.size . T.mkLinks

wheelLinks :: [Triples] -> Set.IntSet
wheelLinks = Set.unions . map (Map.keysSet . T.mkLinks)

reduceWheels :: Int -> [Triples] -> [Triples]
reduceWheels = map . reduceWheel

reduceWheel :: Int -> Triples -> Triples
reduceWheel n
  = T.triplesFromList . concat
  . filter ((>n) . length)
  . Map.elems . T.mkLinks
