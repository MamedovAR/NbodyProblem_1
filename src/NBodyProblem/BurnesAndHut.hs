
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

-- | A node represents a body if it is an endnote (i.e. if node.child is None) or an abstract node of the quad-tree if it has child.
-- Instead of storing the position of a node, we store the mass times position, m_pos. This makes it easier to update the center-of-mass.
data Node = Node { m :: Float
                 , m_pos :: [Float]
                 , momentum :: [Float]
                 , s :: Float
                 , relpos :: [Float]
                 , child :: Maybe [Maybe Node]
                 } deriving(Eq,Show)

-- | The initializer creates a child-less node (an actual body).
initNode :: Float -> Float -> Float -> Node
initNode m x y = Node { m = m
                      , m_pos = [m * x, m * y]
                      , momentum = [0.0, 0.0]
                      , s = 1.0
                      , relpos = [x, y]
                      , child = Nothing
                      }

-- | Places node into next-level quadrant and returns the quadrant number.
intoNextQuadrant :: Node -> ([Node], Int)
intoNextQuadrant node =
  let s' = 0.5 * s node
      subdivide i =
        let relpos' = relpos node
            relpos'' =
              if (relpos' !! i) * 2.0 < 1.0
              then relpos'
              else [if i==0 then 2.0*head relpos' - 1.0 else head relpos', if i==1 then (relpos' !! 1)*2.0 - 1.0 else relpos' !! 1]
        in Node { m = m node
                , m_pos = m_pos node
                , momentum = momentum node
                , s = s'
                , relpos = relpos''
                , child = Nothing
                }
        : _subdivide (node { s = s', relpos = relpos'' }) (if i < 1 then i + 1 else i) 4
      quadrants = subdivide 0 ++ subdivide 1
  in (quadrants, 2 * length quadrants)

-- _subdivide :: Node -> Int -> [Node]
-- _subdivide node i =
--   let relpos' = relpos node
--       relpos'' =
--         if relpos' !! i * 2.0 < 1.0
--         then [if i==0 then 2.0*head relpos' else head relpos', 
--           if i==1 then 2.0*(relpos' !! 1) else relpos' !! 1] 
--         else [if i==0 then 2.0*head relpos' - 1.0 else head relpos', 
--           if i==1 then 2.0*(relpos' !! 1) - 1.0 else relpos' !! 1]
--       s' = 0.5 * s node
--   in Node { m = m node
--           , m_pos = m_pos node
--           , momentum = momentum node
--           , s = s'
--           , relpos = relpos''
--           , child = Nothing
--           }
--      : _subdivide (node { s = s', relpos = relpos'' }) ((i + 1) `mod` 2)

-- | Places node into next-level quadrant along direction i and recomputes the relative position relpos of the node inside this quadrant.
_subdivide :: Node -> Int -> Int -> [Node]
_subdivide node i depth
  | depth <= 0 = []
  | otherwise =
    let relpos' = relpos node
        relpos'' =
          if (relpos' !! i) * 2.0 < 1.0
          then [if i==0 then 2.0*head relpos' else head relpos',
            if i==1 then 2.0*(relpos' !! 1) else relpos' !! 1]
          else [if i==0 then 2.0*head relpos' - 1.0 else head relpos',
            if i==1 then 2.0*(relpos' !! 1) - 1.0 else relpos' !! 1]
        s' = 0.5 * s node
        newNode = Node { m = m node
                       , m_pos = m_pos node
                       , momentum = momentum node
                       , s = s'
                       , relpos = relpos''
                       , child = Nothing
                       }
    in newNode : _subdivide newNode ((i + 1) `mod` 2) (depth - 1)

-- | Physical position of node, independent of currently active quadrant.
pos :: Node -> [Float]
pos node = let [x, y] = m_pos node in [x / m node, y / m node]

-- | Re-positions the node to the level-0 quadrant (full domain).
resetTo0thQuadrant :: Node -> Node
resetTo0thQuadrant node = node { s = 1.0, relpos = pos node }

-- | Distance between present node and another node.
dist :: Node -> Node -> Float
dist n1 n2 = let [x1, y1] = pos n1
                 [x2, y2] = pos n2
             in sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- | Force which the present node is exerting on a given body.
forceOn :: Node -> Node -> Float -> [Float]
forceOn n1 n2 theta =
  let cutoffDist = 0.02
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

-- | Barnes-Hut algorithm: Creation of the quad-tree. This function adds a new body into a quad-tree node. Returns an updated version of the node.
add :: Node -> Maybe Node -> Node --Ошибка кроется здесь!
add body Nothing = body
add body (Just node) =
  let smallestQuadrant = 1.0e-6
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

-- | Execute a time iteration according to the Verlet algorithm.
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
