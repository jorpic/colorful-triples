import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List (foldl')


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
