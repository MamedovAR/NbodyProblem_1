import NBodyProblem.BurnesAndHut
import System.IO.Unsafe

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
		return $ unsafePerformIO $ mainFunc bodies theta g dt (max_iter) (sps++[lst])
