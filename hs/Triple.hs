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

links :: Triple -> [Int]
links (Triple t) = map (.&. 0xffff) [shiftR t 32, shiftR t 16, t]

fromLinks :: Int -> Int -> Int -> Triple
fromLinks a b c
  | a < b && b < c && c < 0x10000
    = Triple $ shiftL a 32 .|. shiftL b 16 .|. c
  | otherwise = error $ "invalid triple " ++ show (a, b, c)


type Triples = Set.IntSet

pyth :: Int -> Triples
pyth n = Set.fromDistinctAscList
  [ asInt $ fromLinks a b c
  |  a <- [2..n-2], b <- [a+1..n-1]
  , let ab = a*a + b*b
  , let (isRoot, c) = maybe (False, -1) (True,) $ exactSquareRoot ab
  , isRoot && c <= n
  ]


type Links = Map.IntMap [Triple]

linksMap :: Triples -> Links
linksMap
  = Set.foldl' insertLinks Map.empty
  where
    insertLinks m x =
      let t = Triple x
      in foldl' (\n k -> Map.insertWith (++) k [t] n) m $ links t
