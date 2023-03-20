import Control.Monad (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
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
    ]

linkMap :: [Triple] -> IntMap [Triple]
linkMap = foldl'
  (\m t -> foldl' (\n k -> Map.insertWith (++) k [t] n) m t)
  Map.empty

dropSingles :: [Triple] -> IO [Triple]
dropSingles = go
  where
    go p = do
      let m = linkMap p
      print ("number of points", Map.size m)

      let s = Map.filter ((== 1) . length) m
      print ("number of points occuring only once", Map.size s)
      if Map.size s > 0
        then go $ filter (all (`Map.notMember` s)) p
        else return p


main :: IO ()
main = do
  print ("number of triples", length py3)

  void $ dropSingles py3

  -- viewRoot 11
  -- viewRoot 19
  -- viewRoot 23
  -- viewRoot 79
  -- viewRoot 83


neighbors :: Int -> Int -> [Triple] -> [Triple]
neighbors depth root xs
  | depth <= 0 = []
  | otherwise = nub $ layer0 ++ layer1
  where
    layer0 = filter (elem root) xs
    layer1 = concat
      $ map (\r -> take 5 $ neighbors (depth-1) r xs)
      $ nub $ concat layer0


viewRoot root = do
  let file = show root ++ ".svg"
  saveGraph file $ toGraph $ neighbors 2 root py3
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


connectedComponents :: [Triple] -> [[Triple]]
connectedComponents xs = []
