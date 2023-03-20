import Control.Monad (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List (foldl', sortBy)
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

py3 :: Int -> [Triple]
py3 m
  = [ [a,b,c]
    | a <- [1..m]
    , b <- [a..m]
    , let ab = a*a + b*b
    , let c = floor $ sqrt $ fromIntegral ab, ab == c*c
    ]

counts :: [Int] -> IntMap Int
counts = foldl' (\m x -> Map.insertWith (+) x 1 m) Map.empty

main :: IO ()
main = do
  let p = py3 7825
  print ("number of triples", length p)

  let cs = counts $ concat p -- Pt => Count
  print ("number of points", Map.size cs)
  let ccs = counts $ Map.elems cs
  print ("number of points occuring only once", ccs Map.! 1)

  mapM_ print
    $ take 20
    $ sortBy (comparing snd) $ Map.toList cs

  void $ viewGraph $ toGraph $ neighbors 85 p


viewGraph g = do
  let file = "graph.svg"
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
  spawnCommand $ "imv -bffffff " ++ file


neighbors :: Int -> [Triple] -> [Triple]
neighbors root = filter (elem root)

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
