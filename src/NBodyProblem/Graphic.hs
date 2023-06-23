{-# LANGUAGE CPP #-}

module NBodyProblem.Graphic where
    
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.Time.Clock.POSIX
import Data.IORef

--showWindow :: [[Float]] -> IO ()
showWindow lst = do
    tstamp <- getTimestamp
    st <- newIORef (0.0, tstamp)
    getArgsAndInitialize
    createAwindow "Alghoritm" st lst
    mainLoop

createAwindow name st lst = do
    createWindow name
    displayCallback $= display st lst
    idleCallback $= Just (idle st)
    mainLoop

display st lst = do
    (dy, _) <- readIORef st
    clear [ColorBuffer]
    clearColor $= Color4 0 0 0 0
    renderPrimitive Points $ mapM_ drawPoint $ [map (\a -> a ++ [0]) x | x <- lst] !! 0
    flush

drawPoint :: [GLfloat] -> IO ()
drawPoint [x, y, z] = do
    currentColor $= Color4 0 0 1 0
    vertex (Vertex3 x y z)

getTimestamp :: IO GLfloat
getTimestamp = do
    now <- getPOSIXTime
    return $ fromRational $ toRational now

idle st = do
    (dy, prevTStamp) <- get st
    tstamp <- getTimestamp
    let dt = tstamp - prevTStamp
        dy' = dy + dt
    writeIORef st (dy', tstamp)
    postRedisplay Nothing
