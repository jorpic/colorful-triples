import Control.Monad (void, forM_, when)
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import qualified Data.Map.Strict as Map'
import qualified Data.Set as Set'

import Data.List (foldl', sortBy, nub)
import Data.Ord (comparing)

import System.Process (spawnCommand)

import Data.GraphViz (quickParams, GraphvizCommand(Dot))
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
  ( mkGraph
  , layoutGraph
  , drawGraph
  )
import Diagrams.Backend.SVG (SVG, renderSVG, B)

type Triple = [Int]

_MAX :: Int
_MAX = 7825

py3 :: [Triple]
py3
  = [ [a,b,c]
    | a <- [1.._MAX]
    , b <- [a.._MAX]
    , let ab = a*a + b*b
    , let c = floor $ sqrt $ fromIntegral ab, ab == c*c
    , c <= _MAX
    ]


main :: IO ()
main = do
  print ("number of triples", length py3)

  py3' <- dropSingles py3

  let [mcPoints] = connectedComponents py3'
  print ("We got single component with", Set.size mcPoints, "points")

  let mc = filter (all (`Set.member` mcPoints)) py3
  print ("Number of triples in this component", length mc)
  let mcm = linkMap mc

  viewRoot 7825 mc

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



linkMap :: [Triple] -> IntMap [Triple]
linkMap = foldl'
  (\m t -> foldl' (\n k -> Map.insertWith (++) k [t] n) m t)
  Map.empty


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


agg (a,f) = foldl' (\m x -> Map.insertWith f x a m) Map.empty


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


viewRoot root xs = do
  let file = show root ++ ".svg"
  saveGraph file $ toGraph $ neighbors 2 5  root xs
  void $ spawnCommand $ "imv -bffffff " ++ file


saveGraph file g = do
  g' <- layoutGraph Dot g
  let node n = text (show n) # fontSizeL 8 <> circle 19
  let d = drawGraph
        (place . node)
        (\_ _ _ _ e p
          -> stroke p # lw veryThin
          <> atPoints (map last $ pathVertices p)
            (repeat $ text (show e) # fontSize 12)
        )
        g'
  renderSVG file (mkSizeSpec $ V2 (Just 800) (Just 1600)) d


toGraph xs = mkGraph xs
  $ concatMap (\l -> [(a, b, l) | (a,b) <- pairs $ g Map.! l])
  $ Map.keys g
  where
    g = linkMap xs

    pairs :: Ord a => [a] -> [(a,a)]
    pairs xs = [(a,b) | a <- xs, b <- xs, a < b]


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
