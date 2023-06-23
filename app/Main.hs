module Main (main) where

import NBodyProblem.BurnesAndHut
import NBodyProblem.Graphic
import NBodyProblem.Console
import System.Random

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
copyCycle body = body{momentum=([-(r !! 1), (r !! 0)]) * mass*inivel*norm(r)/ini_radius} where r = map (-) (pos body) [0.5,0.5]

bodies = map copyCycle [ _init mass px py | (px,py) <- zip posx posy, (px-0.5)^2 + (py-0.5)^2 < ini_radius^2 ]

preBodies :: [Node] -> [IORef Node] -> [IORef Node]
preBodies (x:xs) ys = do
	y <- newIORef x
	if null xs 
		then return (ys++[y]) 
		else return $ preBodies xs (ys++[y])

main :: IO ()
main = do
	bodies' <- preBodies bodies []
	res <- mainFunc bodies' theta _G dt max_iter []
	
