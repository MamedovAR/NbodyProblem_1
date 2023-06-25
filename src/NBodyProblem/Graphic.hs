
{-# LANGUAGE CPP #-}

module Graphic where 
    
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
      Vertex2(Vertex2),
      Color4(Color4),
      Vertex(vertex),
      GLfloat,
      HasSetter(($=)) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import Control.Concurrent (threadDelay)

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
    threadDelay 500000 -- ожидание 0.5 секунды
    flush

drawPoint :: [GLfloat] -> IO ()
drawPoint xs = do
    currentColor $= Color4 0 0 1 0
    vertex (Vertex2 (head xs) (xs!!1))

idle :: IORef [[Float]] -> IORef [[[Float]]] -> IO ()
idle st lst = do
    lst' <- readIORef lst
    writeIORef st $ getFrst lst'
    modifyIORef lst getTail
    postRedisplay Nothing
