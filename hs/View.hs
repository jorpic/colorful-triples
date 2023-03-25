module View where

import Control.Monad (void)
import Data.IntMap qualified as Map
import System.Process (spawnCommand)

import Data.GraphViz (quickParams, GraphvizCommand(Dot))
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
  ( mkGraph
  , layoutGraph
  , drawGraph
  )
import Diagrams.Backend.SVG (SVG, renderSVG, B)

import Types
import GraphAlgos (neighbors, linkMap)


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
