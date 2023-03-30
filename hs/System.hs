{-# LANGUAGE BangPatterns #-}
module System where

import Data.IntSet qualified as Set
import Data.IntMap qualified as Map
import Data.Bits (shiftL, (.|.), (.&.))
import Data.List (foldl')

import Triple (Triple(..), Triples)
import Triple qualified as T



wheelLinks :: Triples -> Set.IntSet
wheelLinks = Set.fromList . concatMap (T.links . Triple) . Set.elems


type Mask = Int

wheelsToSystem :: [Triples] -> (Int, Int, Int)
wheelsToSystem ws
  | n <= 27 = loop 0 0
  | otherwise = error $ "number of links is too big: " ++ show n
  where
    loop !s !x
      | x >= nn = (s, n, nn)
      | otherwise = loop s' (x+1)
      where
        s' = if all (validTriple x) allMasks then s+1 else s

    allLinks = Set.elems $ Set.unions $ map wheelLinks ws
    n = length allLinks
    nn = 2^n
    linkToIx = Map.fromList $ zip allLinks [0..]

    toMask :: Triple -> Mask
    toMask = joinBits . map (shiftL 1 . (linkToIx Map.!)) . T.links

    joinBits = foldl' (.|.) 0

    allMasks = map (toMask . Triple) $ concatMap Set.elems ws

    validTriple :: Int -> Mask -> Bool
    validTriple x m = (x .&. m) /= 0 && (x .&. m) /= m
