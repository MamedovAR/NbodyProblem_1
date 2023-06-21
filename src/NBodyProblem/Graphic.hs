{-# LANGUAGE CPP #-}

module NBodyProblem.Graphic where
    
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

showWindow :: IO ()
showWindow = do
    getArgsAndInitialize
    createAwindow "Alghoritm"
    mainLoop

createAwindow name = do
    createWindow name
    displayCallback $= display

display = do
    clear [ColorBuffer]
    clearColor $= Color4 0 0 0 0
    renderPrimitive Points $ mapM_ drawPoint [(0.1,0.5,0),(0.1,0.2,0),(0.2,0.1,0)]
    flush

drawPoint :: (GLfloat, GLfloat, GLfloat) -> IO ()
drawPoint (x, y, z) = do
    currentColor $= Color4 0 0 1 0
    vertex (Vertex3 x y z)
