
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

{-# LANGUAGE CPP #-}

module NBodyProblem.Graphic where 
    
import Graphics.UI.GLUT
    ( mainLoop,
      idleCallback,
      displayCallback,
      getArgsAndInitialize,
      createWindow,
      postRedisplay,
      renderPrimitive,
      flush,
      clear,
      clearColor,
      currentColor,
      destroyWindow,
      ClearBuffer(ColorBuffer),
      PrimitiveMode(Points),
      Vertex3(Vertex3),
      Color4(Color4),
      Vertex(vertex),
      GLfloat,
      HasSetter(($=)) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import Control.Concurrent (threadDelay)

-- | This function need for visualize with process by graphical interface.

showWindow :: IORef [[[Float]]] -> IO ()
showWindow lst = do
    lst' <- readIORef lst
    st <- newIORef $ head lst'
    _ <- getArgsAndInitialize
    createAwindow "Alghoritm" st lst
    mainLoop

createAwindow :: String -> IORef [[GLfloat]] -> IORef [[[GLfloat]]] -> IO ()
createAwindow name st lst = do
    window <- createWindow name
    displayCallback $= display st
    idleCallback $= Just (idle st lst)
    lst'' <- readIORef lst
    if null lst'' 
        then destroyWindow window
        else mainLoop

getFrst :: [[a]] -> [a]
getFrst [] = []
getFrst xs = head xs

getTail :: [[a]] -> [[a]]
getTail [] = []
getTail xs = tail xs

display :: IORef [[Float]] -> IO ()
display st = do
    xs <- readIORef st
    clear [ColorBuffer]
    clearColor $= Color4 0 0 0 0
    renderPrimitive Points $ mapM_ drawPoint xs
    threadDelay 10000 -- ожидание 0.5 секунды
    flush

drawPoint :: [GLfloat] -> IO ()
drawPoint xs = do
    currentColor $= Color4 0 0 1 0
    vertex (Vertex3 (head xs - 0.5) (xs!!1 - 0.5) 0.0)

idle :: IORef [[Float]] -> IORef [[[Float]]] -> IO ()
idle st lst = do
    lst' <- readIORef lst
    writeIORef st $ getFrst lst'
    modifyIORef lst getTail
    postRedisplay Nothing
