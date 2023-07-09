module Main where

import NBodyProblem.BurnesAndHut
import NBodyProblem.Graphic
import NBodyProblem.Console
import System.Environment
import System.Random
import Data.IORef (newIORef)

norm :: [Float] -> Float
norm xs = sqrt $ sum $ map (^2) xs

-- Theta-criterion of the Barnes-Hut algorithm.
theta :: Float
theta = 0.5
-- Initially, the bodies are distributed inside a circle of radius ini_radius.
ini_radius :: Float
ini_radius = 0.5
-- Initial maximum velocity of the bodies.
inivel :: Float
inivel = 0.1
-- The "gravitational constant" is chosen so as to get a pleasant output.
_G :: Float
_G = 0.000004
-- Discrete time step.
dt :: Float
dt = 0.001
-- Number of bodies (the actual number is smaller, because all bodies
-- outside the initial radius are removed).
numbodies :: Int
numbodies = 1000
-- Number of time-iterations executed by the program.
max_iter :: Int
max_iter = 10000
-- -- Frequency at which PNG images are written.
-- img_iter :: Integer
-- img_iter = 20

-- The pseudo-random number generator is initialized at a deterministic -- value, for proper validation of the output for the exercise series.  random.seed(1)
-- x- and y-pos are initialized to a square with side-length 2*ini_radius.
posx :: [Float]
posx = map (\x -> x*2*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 529) :: [Float]--random.random(numbodies) 
posy :: [Float]
posy = map (\x -> x*2*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 1000) :: [Float]--random.random(numbodies) *2.*ini_radius + 0.5-ini_radius
-- Mass of a body.
mass :: [Float]
mass = map abs $ take numbodies $ repeat 1--randoms (mkStdGen 432) :: [Float]
-- We only keep the bodies inside a circle of radius ini_radius.
--copyCycle body = body{momentum=(map (\t -> t * mass*inivel*(norm r)/ini_radius) [-(r !! 1), (r !! 0)])} where r = zipWith (-) (pos body) [0.5,0.5]

bodies :: [Node]
bodies = [initNode 1000000 0.9 0.9]++[ initNode ms px py | (ms,px,py) <- zip3 mass posx posy, (px-0.5)^2 + (py-0.5)^2 < ini_radius^2 ]

help :: String
help = "Simple Haskell implementation of a Barnes-Hut galaxy simulator.\n\n./NBodyProblem [OPTION] [OUTPUT]\n\n\t-h, --help - Show supported options.\n" ++ 
	"\t-o, --out OUTPUT - write all iterations in one csv-file.\n"

createStr :: [[[Float]]] -> String
createStr lst = (init $ concat [show x ++ "x," ++ show x ++ "y," ++ show x ++ "z," | x <- [0..length lst]]) ++ 
	"\n" ++ (unlines $ map (\lst1 -> init $ concat [show x ++ "," ++ show y ++ ",0," | [x,y] <- lst1]) lst)

main :: IO ()
main = do
	args <- getArgs
	if length args == 1 then do{putStrLn help} else do{putStrLn ""}
	if null bodies then putStrLn "Not bodies" else putStrLn "Bodies exist"
	res <- mainFunc bodies theta _G dt max_iter inivel ini_radius
	if length args == 2 then do{writeFile (last args) $ createStr res} else do{putStrLn ""}
	resRef <- newIORef res
	showWindow resRef
	return ()
