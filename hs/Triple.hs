module Triple where

import Data.List (foldl')
import Data.IntMap.Strict qualified as Map
import Data.IntSet qualified as Set
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

import Math.NumberTheory.Roots (exactSquareRoot)
--  ^^^-- this is three times faster than `floor . sqrt :: Float`

newtype Triple = Triple { asInt :: Int }
  deriving (Eq, Ord)

instance Show Triple where
  show = show . links

type Triples = Set.IntSet
type Links = Map.IntMap [Triple]

data Graph = Graph
  { graphTriples :: Triples
  , graphLinks :: Links
  }

mkGraph :: Triples -> Graph
mkGraph ts = Graph ts (mkLinks ts)

graphFromTripleList :: [Triple] -> Graph
graphFromTripleList = mkGraph . triplesFromList

triplesFromList :: [Triple] -> Triples
triplesFromList = Set.fromList . map asInt

graphTriplesList :: Graph -> [Triple]
graphTriplesList = map Triple . Set.toList . graphTriples

mkLinks :: Triples -> Links
mkLinks
  = Set.foldl' insertLinks Map.empty
  where
    insertLinks m x =
      let t = Triple x
      in foldl' (\n k -> Map.insertWith (++) k [t] n) m $ links t

links :: Triple -> [Int]
links (Triple t) = map (.&. 0xffff) [shiftR t 32, shiftR t 16, t]

mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c
  | a < b && b < c && c < 0x10000
    = Triple $ shiftL a 32 .|. shiftL b 16 .|. c
  | otherwise = error $ "invalid triple " ++ show (a, b, c)



pyth :: Int -> Graph
pyth n = mkGraph $ Set.fromDistinctAscList
  [ asInt $ mkTriple a b c
  |  a <- [2..n-2], b <- [a+1..n-1]
  , let ab = a*a + b*b
  , let (isRoot, c) = maybe (False, -1) (True,) $ exactSquareRoot ab
  , isRoot && c <= n
  ]


