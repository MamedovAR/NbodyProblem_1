
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module NBodyProblem.Console where

import NBodyProblem.BurnesAndHut
import System.Process (system)
import GHC.IO.Exception (ExitCode)
-- import Control.Monad ( when )
-- import Graphics.UI.GLUT (DataType(Float))
--import Main (bodies)

clearConsole :: IO ExitCode
clearConsole = system "clear"--putStr "\ESC[2J"

showLine :: String -> Int -> IO ()
showLine ln i = do
        if i==(-20) then do {return ()} else do
                putStrLn $ "|" ++ take 20 (drop i ln) ++ "|"
                showLine ln (i-20)

showLst :: [[Float]] -> IO ()
showLst lst = do
        let lst' = map ((\a -> (a!!0)*20 + (a!!1)) . map (\a -> round $ a*20)) (if length lst > 20 then take 20 lst else lst)
        let output = [if a `elem` lst' then '.' else ' ' | a <- [0..399]]
        _ <- clearConsole
        putStrLn "+--------------------+"
        showLine output 380
        putStrLn "+--------------------+"
        return ()

createList :: [Node] -> [[Float]] -> [[Float]]
createList [] lst = lst
createList bodies lst = createList (tail bodies) (lst++[m_pos $ head bodies])
-- createList bodies lst = do
--         if null bodies then return lst else do
--                 let body = head bodies
--                 return $ unsafePerformIO $ createList (tail bodies) (lst++[m_pos body])

mainFunc :: [Node] -> Float -> Float -> Float -> Int -> Float -> Float -> Float -> IO [[[Float]]]
mainFunc bodies theta g dt maxIter mass inivel iniRadius = do
        let updateBody' body =
                let [rx, ry] = let [x, y] = m_pos body in [x - 0.5, y - 0.5]
                    [vx, vy] = [-ry, rx]
                in body { momentum = [vx * mass * inivel * sqrt (rx ** 2 + ry ** 2) / iniRadius, vy * mass * inivel * sqrt (rx ** 2 + ry ** 2) / iniRadius] }
        let bodies' = map updateBody' bodies
        putStrLn "Some preparing"
        let loop :: Int -> [Node] -> [[[Float]]] -> IO [[[Float]]]
            loop i bs lst = do
                if i >= maxIter
                then return lst
                else do
                        let root = fromJust $ foldl (flip (\a b -> Just $ add a b)) Nothing bs
                        let bs' = verlet bs root theta g dt
                        let lst' = createList bs' []
--                        if length lst' == length lst then putStrLn "In loop preparing" else putStrLn "In loop"
--                        print root
                        showLst lst'
                        loop (i + 1) bs' (lst++[lst'])
        loop 0 bodies' []
--         if max_iter==0 then do {return sps} else do
--                 root <- newIORef NoneNode
--                 root' <- rootInit root bodies
--                 root'' <- readIORef root'
-- --		let bodies' = map (\b -> unsafePerformIO $ resetTo0thQuadrant b >> add b root) bodies
--                 verlet bodies root'' theta g dt
--                 lst <- createList bodies []
--                 clearConsole
--                 showLst lst-- 380
--                 return $ unsafePerformIO $ mainFunc bodies theta g dt (max_iter) (sps++[lst])

-- rootInit :: IORef Node -> [IORef Node] -> IO (IORef Node)
-- rootInit root [body] = do
--         resetTo0thQuadrant body
--         res <- add body root
--         return res
-- rootInit root (body:bodies) = do
--         resetTo0thQuadrant body
--         res <- add body root
--         rootInit res bodies
