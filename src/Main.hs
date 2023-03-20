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

counts :: [Int] -> IntMap Int
counts = foldl' (\m x -> Map.insertWith (+) x 1 m) Map.empty


main :: IO ()
main = do
  print ("number of triples", length py3)

  let cs = counts $ concat py3 -- Pt => Count
  print ("number of points", Map.size cs)
  let ccs = counts $ Map.elems cs
  print ("number of points occuring only once", ccs Map.! 1)

  mapM_ print
    $ take 20
    $ sortBy (comparing snd) $ Map.toList cs

  viewRoot 11
  viewRoot 19
  viewRoot 23
  viewRoot 79
  viewRoot 83


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
    g = fromTriples xs

    pairs :: Ord a => [a] -> [(a,a)]
    pairs xs = [(a,b) | a <- xs, b <- xs, a < b]

    fromTriples :: [Triple] -> IntMap [Triple]
    fromTriples = foldl' (\m t
      -> Map.insertWith (++) (t!!0) [t]
      $  Map.insertWith (++) (t!!1) [t]
      $  Map.insertWith (++) (t!!2) [t]
      m) Map.empty
