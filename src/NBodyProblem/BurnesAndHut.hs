
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

module BurnesAndHut where
  
import Data.IORef
import System.IO.Unsafe

replace xs a n = (take n xs) ++ [a] ++ (drop (n+1) xs)

-- | A node represents a body if it is an endnote (i.e. if node.child is None)
-- or an abstract node of the quad-tree if it has child.

data Node = NoneNode | Node {
 m :: Float,
 m_pos :: [Float],
 momentum :: [Float],
 child :: [Node],
 s :: Float,
 relpos :: [Float]
} deriving (Show,Read)

-- | The initializer creates a child-less node (an actual body).
_init :: Float -> Float -> Float -> Node
_init k x y = Node {m=k,m_pos=[x,y],momentum=[0,0],child=[],s=1,relpos=[x/k,y/k]}

-- | Physical position of node, independent of currently active quadrant.
pos :: Node -> [Float]
pos n = [((m_pos n) !! 0)/(m n),((m_pos n) !! 1)/(m n)]

custoffDist :: Float
custoffDist = 0.002

-- | Distance between present node and another node.
dist :: Node -> Node -> Float
dist n1 n2 = sqrt((((pos n1)!!0)-((pos n2)!!0))^2+(((pos n1)!!1)-((pos n2)!!1))^2)

-- | Force which the present node is exerting on a given body.
forceOn :: Node -> Node -> [Float]
forceOn n1 n2 = if d<custoffDist then [0,0] else map (\x -> x*((m n1)*(m n2))^3) $ zipWith (-) (pos n1) (pos n2)
 where d=dist n1 n2

-- | Places node into next-level quadrant along direction i and recomputes
-- the relative position relpos of the node inside this quadrant.
_subdivide :: IORef Node -> Int -> IO Int
_subdivide n i = do
 n1 <- readIORef n
 if (2*((relpos n1)!!i))<1 then do
  writeIORef n n1{relpos=replace (relpos n1) (2*((relpos n1)!!i)) i}
  return 0 
 else do
  writeIORef n n1{relpos=replace (relpos n1) (2*((relpos n1)!!i)-1) i}
  return 1
  
-- | Places node into next-level quadrant and returns the quadrant number.
intoNextQuadrant :: IORef Node -> IO Int
intoNextQuadrant n = do
 modifyIORef n n1
 a <- (_subdivide n 1)
 b <- (_subdivide n 0)
 let c = ((a) + 2*(b))
 return c
 where n1 n2 = n2{s=0.5*(s n2)}

-- | Re-positions the node to the level-0 quadrant (full domain).
resetTo0thQuadrant :: IORef Node -> IO ()
resetTo0thQuadrant n = do
 n1 <- readIORef n 
 modifyIORef n (\n2 -> n2{s=1,relpos=pos n1})

-- | @isNoneNode node@ check is it node NULL.
isNoneNode :: Node -> Bool
isNoneNode NoneNode = True
isNoneNode _ = False

-- | Barnes-Hut algorithm: Creation of the quad-tree. This function adds 
-- a new body into a quad-tree node. Returns an updated version of the node.
add :: IORef Node -> IORef Node -> IO (IORef Node)
add body node = do
 body' <- readIORef body
 node' <- readIORef node
 nonen' <- newIORef NoneNode
 let new_node = if isNoneNode node' then body else nonen'
 let smallest_quadrant = 0.0001
 if not $ isNoneNode node' && s node' > smallest_quadrant then do
  if null $ child node' then do
   writeIORef new_node node'
   modifyIORef new_node (\n_node -> n_node{child=replicate 4 NoneNode})
   quadrant <- intoNextQuadrant node
   modifyIORef new_node (\x -> x{child=replace (child x) node' quadrant})
  else do
   writeIORef new_node node'
  modifyIORef new_node (\n_node -> n_node{m=(m n_node)+ (m body')})
  modifyIORef new_node (\n_node -> n_node{m_pos=(zipWith (+) (m_pos n_node) (m_pos body'))})
  quadrant <- intoNextQuadrant body
  new_node' <- readIORef new_node
  node1' <- newIORef ((child new_node') !! quadrant)
  nncq <- add body node1'
  nncq' <- readIORef nncq
  modifyIORef new_node (\n_node -> n_node{child=replace (child new_node') nncq' quadrant})
  return new_node
 else 
  return new_node

-- | Barnes-Hut algorithm: usage of the quad-tree. This function computes
-- the net force on a body exerted by all bodies in node "node".
-- Note how the code is shorter and more expressive than the human-language
-- description of the algorithm.
forceOn1 :: Node -> Node -> Float -> [Float]
forceOn1 body node theta
 | null $ child node = forceOn (node) body
 | s (node) < (dist (node) body)*theta = forceOn (node) body
 | otherwise = [sum(forceOn1 body c theta) | c <- child node, not $ isNoneNode c]

-- | Execute a time iteration according to the Verlet algorithm.
verlet :: [IORef Node] -> Node -> Float -> Float -> Float -> IO ()
verlet bodies root theta g dt = 
 if null bodies then return () else do
  let body = head bodies
  body' <- readIORef body
  let force = map (\a -> a*g) $ forceOn1 body' root theta
  modifyIORef body (\a -> a{momentum=zipWith (+) (momentum a) (map (\a -> dt*a) force)})
  body'' <- readIORef body
  modifyIORef body (\a -> a{momentum=zipWith (+) (m_pos a) $ map (\b -> b*dt) $ momentum body''})
  return $ unsafePerformIO $ verlet (tail bodies) root theta g dt
