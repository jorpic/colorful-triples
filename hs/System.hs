{-# LANGUAGE BangPatterns #-}
module System where

import Data.IntSet qualified as Set
import Data.IntMap qualified as Map
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (foldl')

import Triple (Triple(..), Triples)
import Triple qualified as T



wheelLinks :: Triples -> Set.IntSet
wheelLinks = Set.fromList . concatMap (T.links . Triple) . Set.elems


data System = System
  { varToIx :: Map.IntMap Int
  , solutions :: Set.IntSet
  }

instance Show System where
  show (System{..})
    =  "number of vars: " ++ show n
    ++ ", soultions: "
      ++ show s ++ "/"
      ++ show (2^n) ++ "/" ++ show (2^n `div` s)
    where
      n = Map.size varToIx
      s = Set.size solutions

type Mask = Int


wheelsToSystem :: [Triples] -> System
wheelsToSystem ws
  | n <= 49 = System linkToIx $ Set.fromDistinctAscList $ loop 0
  | otherwise = error $ "number of links is too big: " ++ show n
  where
    loop !x
      | x >= nn = []
      | all (validTriple x) allMasks = x : loop (x+1)
      | otherwise = loop (x+1)

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


join :: System -> System -> (System, System)
join a b
  = ( a {solutions = Set.filter (flip Set.member ab . extract aVars) $ solutions a}
    , b {solutions = Set.filter (flip Set.member ab . extract bVars) $ solutions b}
    )
  where
    aVars = Map.intersection (varToIx a) (varToIx b)
    bVars = Map.intersection (varToIx b) (varToIx a)

    as = Set.map (extract aVars) $ solutions a
    bs = Set.map (extract bVars) $ solutions b

    ab = Set.intersection as bs

    extract :: Map.IntMap Int -> Int -> Int
    extract vars x
      = foldl' (\r (_,i) -> shiftL r 1 .|. (shiftR x i .&. 1)) 0
      $ Map.toAscList vars
