-----------------------------------------------------------------------------
-- |
-- Module      :  NBodyProblem.BurnesAndHut
-- Copyright   :  (c) Artem Mamedov (2023)
-- License     :  LGPL-style (see the LICENSE file)
--
-- Maintainer  :  a.mamedov1@g.nsu.ru
-- Stability   :  provisional
-- Portability :  portable
--
-- The simple Haskell implementation of a Barnes-Hut galaxy simulator.
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module NBodyProblem.BurnesAndHut where

import Data.List(foldl')

data Node = Node { m :: Float
                 , m_pos :: [Float]
                 , momentum :: [Float]
                 , s :: Float
                 , relpos :: [Float]
                 , child :: Maybe [Maybe Node]
                 } deriving(Eq,Show)

initNode :: Float -> Float -> Float -> Node
initNode m x y = Node { m = m
                      , m_pos = [m * x, m * y]
                      , momentum = [0.0, 0.0]
                      , s = 1.0
                      , relpos = [x, y]
                      , child = Nothing
                      }

intoNextQuadrant :: Node -> ([Node], Int)
intoNextQuadrant node =
  let s' = 0.5 * s node
      subdivide i =
        let relpos' = relpos node
            relpos'' =
              if relpos' !! i * 2.0 < 1.0
              then relpos'
              else [head relpos' - 1.0, (relpos' !! 1) - 1.0]
        in Node { m = m node
                , m_pos = m_pos node
                , momentum = momentum node
                , s = s'
                , relpos = relpos''
                , child = Nothing
                }
        : _subdivide (node { s = s', relpos = relpos'' }) (i + 1)
      quadrants = subdivide 0 ++ subdivide 1
  in (quadrants, 2 * length quadrants)

_subdivide :: Node -> Int -> [Node]
_subdivide node i =
  let relpos' = relpos node
      relpos'' =
        if relpos' !! i * 2.0 < 1.0
        then relpos'
        else [head relpos' - 1.0, (relpos' !! 1) - 1.0]
      s' = 0.5 * s node
  in Node { m = m node
          , m_pos = m_pos node
          , momentum = momentum node
          , s = s'
          , relpos = relpos''
          , child = Nothing
          }
     : _subdivide (node { s = s', relpos = relpos'' }) ((i + 1) `mod` 2)

pos :: Node -> [Float]
pos node = let [x, y] = m_pos node in [x / m node, y / m node]

resetTo0thQuadrant :: Node -> Node
resetTo0thQuadrant node = node { s = 1.0, relpos = pos node }

dist :: Node -> Node -> Float
dist n1 n2 = let [x1, y1] = pos n1
                 [x2, y2] = pos n2
             in sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

forceOn :: Node -> Node -> Float -> [Float]
forceOn n1 n2 theta =
  let cutoffDist = 0.002
      d = dist n1 n2
      f =
        if d < cutoffDist
        then [0.0, 0.0]
        else let [x1, y1] = pos n1
                 [x2, y2] = pos n2
                 m1 = m n1
                 m2 = m n2
                 f' = [x1 - x2, y1 - y2]
                 f'' = m1 * m2 / d ** 3
             in [f'' * head f', f'' * (f' !! 1)]
  in if child n2 == Nothing || s n2 < dist n1 n2 * theta
     then f
     else let Just children = child n2
              fs = map (\c -> forceOn n1 (fromJust c) theta) children
          in foldl' (\[x, y] [x', y'] -> [x + x', y + y']) [0.0, 0.0] fs

add :: Node -> Maybe Node -> Node
add body Nothing = body
add body (Just node) =
  let smallestQuadrant = 1.0e-4
      node' =
        if s node > smallestQuadrant
        then case child node of
               Nothing ->
                 let (quadrants, len) = intoNextQuadrant node
                     node'' = node { child = Just $ replicate len Nothing }
                 in node'' { child = Just $ zipWith (\ q i -> (if q == quadrants !! 1 then Just $ add body Nothing else i)) quadrants (fromJust $ child node'') }
               Just children ->
                 let quadrants = map (\c -> if c == Nothing then add body Nothing else fromJust c) children
                 in node { child = Just $ map Just quadrants }
        else node
  in node' { m = m node + m body
           , m_pos = let [x, y] = m_pos node in [x + head (m_pos body), y + m_pos body !! 1]
           }

fromJust :: Maybe a -> a
fromJust (Just x) = x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

verlet :: [Node] -> Node -> Float -> Float -> Float -> [Node]
verlet bodies root theta g dt =
  let force body = map (g *) $ forceOn body root theta
      updateBody body =
        let f = force body
            [px, py] = m_pos body
            [vx, vy] = momentum body
        in body { momentum = [vx + dt * head f, vy + dt * (f !! 1)]
                , m_pos = [px + dt * (vx + 0.5 * dt * head f), py + dt * (vy + 0.5 * dt * (f !! 1))]
                }
  in map updateBody bodies