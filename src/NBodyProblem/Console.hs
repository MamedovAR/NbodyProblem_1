module NBodyProblem.Console where

import NBodyProblem.BurnesAndHut
import System.IO.Unsafe
import System.Cmd

#ifdef _WIN32
clearConsole = system "clr"
#else
clearConsole = system "clear"
#endif

showList :: [[Float]] -> IO ()
showList lst = do
	let lst' = map (\a -> (a!!0)*20 + (a!!1)) $ map (map (\a -> round $ a*20)) $ take 20 lst
	let output = [if elem a lst' then '.' else ' ' | a <- [0..399]]
	let showLine ln i = do
		if i==-20 then return () else
			putStrLn $ take 20 $ drop i
			return $ unsafePerformIO $ showLine ln (i-20)
	showLine output 380
	return ()

createList :: [IORef Node] -> [[Float]] -> IO [[Float]]
createList bodies lst = do
	if bodies == [] then return lst else
		body <- readIORef $ head bodies
		return $ unsafePerformIO $ createList (tail bodies) (lst++[m_pos body])

mainFunc :: [IORef Node] -> Float -> Float -> Float -> Int -> [[[Float]]] -> IO [[[Float]]]
mainFunc bodies theta g dt max_iter sps = do
	if max_iter==0 then return sps else
		let root = NoneNode
		let bodies' = map (\b -> unsafePerformIO $ resetTo0thQuadrant b >> add b root) bodies
		verlet bodies root theta g dt
		lst <- createList bodies []
		clearConsole
		showList lst 380
		return $ unsafePerformIO $ mainFunc bodies theta g dt (max_iter) (sps++[lst])
