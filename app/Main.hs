module Main (main) where

import NBodyProblem.BurnesAndHut
import NBodyProblem.Graphic
import NBodyProblem.Console
import System.Environment
import System.Random
import Data.IORef

norm :: [Float] -> Float
norm xs = sqrt $ sum $ map (^2) xs

-- Theta-criterion of the Barnes-Hut algorithm.
theta = 0.5
-- Mass of a body.
mass = 1.0
-- Initially, the bodies are distributed inside a circle of radius ini_radius.
ini_radius = 0.1
-- Initial maximum velocity of the bodies.
inivel = 0.1
-- The "gravitational constant" is chosen so as to get a pleasant output.
_G = 0.000004
-- Discrete time step.
dt = 0.001
-- Number of bodies (the actual number is smaller, because all bodies
-- outside the initial radius are removed).
numbodies = 1000
-- Number of time-iterations executed by the program.
max_iter = 10000
-- Frequency at which PNG images are written.
img_iter = 20

-- The pseudo-random number generator is initialized at a deterministic -- value, for proper validation of the output for the exercise series.  random.seed(1)
-- x- and y-pos are initialized to a square with side-length 2*ini_radius.
posx = map (\x -> x*2*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 13) :: [Float]--random.random(numbodies) 
posy = map (\x -> x*2*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 13) :: [Float]--random.random(numbodies) *2.*ini_radius + 0.5-ini_radius
-- We only keep the bodies inside a circle of radius ini_radius.
copyCycle body = body{momentum=(map (\t -> t * mass*inivel*(norm r)/ini_radius) [-(r !! 1), (r !! 0)])} where r = zipWith (-) (pos body) [0.5,0.5]

bodies = map copyCycle [ _init mass px py | (px,py) <- zip posx posy, (px-0.5)^2 + (py-0.5)^2 < ini_radius^2 ]

preBodies :: [Node] -> [IORef Node] -> IO [IORef Node]
preBodies (x:xs) ys = do
	y <- newIORef x
	if null xs 
		then return (ys++[y]) 
		else return $ unsafePerformIO $ preBodies xs (ys++[y])

help :: String
help = "Simple Haskell implementation of a Barnes-Hut galaxy simulator.\n\n./NBodyProblem [OPTION] [OUTPUT]\n\n\t-h, --help - Show supported options.\n" ++ 
	"\t-o, --out OUTPUT - write all iterations in one csv-file.\n"

createStr :: [[[Float]]] -> String
createStr lst = (init $ concat [show x ++ "x," ++ show x ++ "y," ++ show x "z," | x <- [0..length lst]]) ++ 
	"\n" ++ unlines $ map (\lst1 -> init $ concat [show x ++ "," ++ show y ++ ",0," | [x,y] <- lst1]) lst

main :: IO ()
main = do
	args <- getArgs
	if length args == 1 then do{putStrLn help} else do{putStrLn ""}
	bodies' <- preBodies bodies []
	res <- mainFunc bodies' theta _G dt max_iter []
	if length args == 2 then do{writeFile (last args) $ createStr res} else do{putStrLn ""}
