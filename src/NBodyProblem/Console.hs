{-# LANGUAGE CPP #-}

module NBodyProblem.Console where

import NBodyProblem.BurnesAndHut
import System.IO.Unsafe
import System.Cmd
import Data.IORef

#ifdef _WIN32
clearConsole = system "clr"
#else
clearConsole = system "clear"
#endif

showLine :: String -> Int -> IO ()
showLine ln i = do
	if i==(-20) then do{return ()} else do
		putStrLn $ take 20 $ drop i ln
		return $ unsafePerformIO $ showLine ln (i-20)

showLst :: [[Float]] -> IO ()
showLst lst = do
	let lst' = map (\a -> (a!!0)*20 + (a!!1)) $ map (map (\a -> round $ a*20)) $ take 20 lst
	let output = [if elem a lst' then '.' else ' ' | a <- [0..399]]
	showLine output 380
	return ()

createList :: [IORef Node] -> [[Float]] -> IO [[Float]]
createList bodies lst = do
	if (null bodies) then return lst else do
		body <- readIORef $ head bodies
		return $ unsafePerformIO $ createList (tail bodies) (lst++[m_pos body])

mainFunc :: [IORef Node] -> Float -> Float -> Float -> Int -> [[[Float]]] -> IO [[[Float]]]
mainFunc bodies theta g dt max_iter sps = do
	if max_iter==0 then do{return sps} else do
		root <- newIORef NoneNode
		rootInit root bodies
		root' <- readIORef root
--		let bodies' = map (\b -> unsafePerformIO $ resetTo0thQuadrant b >> add b root) bodies
		verlet bodies root' theta g dt
		lst <- createList bodies []
		clearConsole
		showLst lst-- 380
		return $ unsafePerformIO $ mainFunc bodies theta g dt (max_iter) (sps++[lst])

rootInit :: IORef Node -> [IORef Node] -> IO ()
rootInit root (body:bodies) = do
	if null bodies then
		resetTo0thQuadrant body
		add body root
	else
		resetTo0thQuadrant body
		add body root
		rootInit root bodies