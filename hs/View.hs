module View where

import Control.Monad (void)
import Data.IntMap qualified as Map
import Data.IntSet qualified as Set
import Data.List (intersect)

import System.Process (spawnCommand)

import Diagrams.Prelude hiding (view)
import Diagrams.TwoD.Text (Text)
import Diagrams.Backend.SVG (SVG, renderSVG, B)

import Triple (Triples, Triple(..))
import Triple qualified as T


view :: String -> Diagram B -> IO ()
view name diag = do
  let file = name ++ ".svg"
  renderSVG file (mkSizeSpec $ V2 (Just 800) (Just 1600)) diag
  void $ spawnCommand $ "imv -bffffff " ++ file

viewWheels :: String -> [Triples] -> IO ()
viewWheels name = view name . wheelsDiag

wheelsDiag :: [Triples] -> Diagram B
wheelsDiag ws = hcat $ map wheelDiag $ takeWhile ((<30) . Set.size) ws

wheelDiag :: Triples -> Diagram B
wheelDiag triples = nodes
  where
    n = Set.size triples
    ts = map Triple $ Set.toList triples

    node :: Triple -> Diagram B
    node t = vcat
      [ text (show l) # fontSizeL 1.6 # fc black === strutY 1.3
      | l <- T.links t
      ]
      <> circle 3 # lw veryThin # named (asInt t)

    nodes
      = atPoints
        (trailVertices $ regPoly n 20)
        (map node ts)
      # applyAll
        [ connectOutside' linkOptions (asInt a) (asInt b)
        | a <- ts, b <- ts
        , not $ null $ intersect (T.links a) (T.links b)
        ]

    linkOptions = with
      & gaps .~ verySmall
      & shaftStyle %~ lw veryThin
      & headLength .~ 0
