import Data.IntSet (IntSet)
import Data.IntSet qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map

import Data.Map.Strict qualified as Map'
import Data.Set qualified as Set'
import Data.List (foldl', sort)

import System.CPUTime (getCPUTime)

import Types
import View (viewRoot)
import GraphAlgos (linkMap, connectedComponents)


_MAX :: Int
_MAX = 7825

py3 :: [Triple]
py3
  = [ [a,b,c]
    | a <- [1.._MAX]
    , b <- [a.._MAX]
    , let ab = a*a + b*b
    , let c = floor $ sqrt $ fromIntegral ab
    , c <= _MAX && ab == c*c
    ]

timeIt f = do
  s <- getCPUTime
  r <- f
  e <- getCPUTime
  print $ (fromIntegral $ e - s) / (10^9)
  return r


type Path = [Int]
type Layer = [(Path, [Triple])]

-- tripleNeighbors :: Int -> Triple -> IntMap [Triple] -> [Layer]
pathsFromTriple depth root g
  = map fst $ loop 1 $ nextLayer [] root
  where
    loop d layer
      | d >= depth = layer
      | otherwise = loop
        (d+1)
        $ filter (\(x:_, _) -> x `Map.member` arcs) l2
      where
        l2 = concat [nextLayer p tpl | (p, tpls) <- layer, tpl <- tpls]
        arcs = arcsFromPaths $  map fst l2

    nextLayer path tpl
      = [ (x:path, filter (/=tpl) $ g Map.! x)
        | x <- tpl
        , not $ x `elem` path
        ]

arcsFromPaths
  = Map.filter ((>1) . length)
  . foldl' (\m p@(x:_) -> Map.insertWith (++) x [p] m) Map.empty


main :: IO ()
main = do
  timeIt
    $ print ("number of triples", length py3)

  py3' <- timeIt $ dropSingles py3


-- [44,240,244]
-- [88,480,488]
-- [625,7800,7825]




  let [mcPoints] = connectedComponents py3'
  print ("We got single component with", Set.size mcPoints, "points")

  let mc = filter (all (`Set.member` mcPoints)) py3
  print ("Number of triples in this component", length mc)
  let mcm = linkMap mc

  -- viewRoot 7825 mc
  --
  timeIt $ do
    let shortArcs = sort
          [ (t, length arcs)
          | t <- mc
          , let arcs = arcsFromPaths (pathsFromTriple 4 t mcm)
          , not $ null arcs
          ]
    mapM_ print shortArcs
    print $ length shortArcs

  -- a lot of points has small weight
  print ("distribution of point weights (weight, count)")
  mapM_ print $ take 16 $ Map.toList $ agg (1,(+)) $ Map.elems $ Map.map length mcm

  -- It is not possible to remove some point from the mainComponent to split
  -- it into several components. (no bridges)
  -- forM_ (Map.keys mcm) $ \k -> do
  --   let cc' = connectedComponents $ filter (all (/=k)) mc
  --   when (length cc' > 1)
  --     $ print (k, length $ mcm Map.! k, map Set.size cc')

  let mpls = joinPoints mcPoints
  print ("number of points without multiplies", Map.size mpls) -- 551
  -- mapM_ print $ Map.toList mpls

  let mtls = joinTriples mc
  print ("number of triples without multiples", Map'.size mtls) -- 1545
  --mapM_ print $ Map'.toList mtls



joinPoints :: IntSet -> IntMap [Int]
joinPoints = loop Map.empty . Set.deleteFindMin
  where
    loop m (x, s)
      | Set.null s = m
      | Set.null s' = m'
      | otherwise = loop m' $ Set.deleteFindMin s'
      where
        xMul = filter ((`Set.member` s) . snd)
          $ zip [1..] $ takeWhile (<=_MAX) $ iterate (+x) x
        m' = Map.insert x (map fst xMul) m
        s' = Set.difference s (Set.fromList $ map snd xMul)


joinTriples :: [Triple] -> Map'.Map Triple [Int]
joinTriples = loop Map'.empty . Set'.deleteFindMin . Set'.fromList
  where
    loop m (x, s)
      | Set'.null s = m
      | Set'.null s' = m'
      | otherwise = loop m' $ Set'.deleteFindMin s'
      where
        xMul = filter ((`Set'.member` s) . snd)
          $ zip [1..] $ takeWhile (all (<=_MAX)) $ iterate (zipWith (+) x) x
        m' = Map'.insert x (map fst xMul) m
        s' = Set'.difference s (Set'.fromList $ map snd xMul)


dropSingles :: [Triple] -> IO [Triple]
dropSingles = go
  where
    -- FIXME: Writer monad to log stats
    go p = do
      let m = linkMap p
      print ("number of points", Map.size m)

      let s = Map.filter ((== 1) . length) m
      print ("number of points occuring only once", Map.size s)
      if Map.size s > 0
        then go $ filter (all (`Map.notMember` s)) p
        else return p


agg :: Foldable t => (a, a -> a -> a) -> t Map.Key -> IntMap a
agg (a,f) = foldl' (\m x -> Map.insertWith f x a m) Map.empty
