module Main (main) where

import NBodyProblem.BurnesAndHut
import NBodyProblem.Graphic
import System.Random
import Numeric.LinearAlgebra

norm :: [Float] -> Float
norm v = norm_2 $ fromList v

-- Theta-criterion of the Barnes-Hut algorithm.
theta = 0.5
-- Mass of a body.
mass = 1.0
-- Initially, the bodies are distributed inside a circle of radius ini_radius.
ini_radius = 0.1
-- Initial maximum velocity of the bodies.
inivel = 0.1
-- The "gravitational constant" is chosen so as to get a pleasant output.
_G = 4.e-6
-- Discrete time step.
dt = 1.e-3
-- Number of bodies (the actual number is smaller, because all bodies
-- outside the initial radius are removed).
numbodies = 1000
-- Number of time-iterations executed by the program.
max_iter = 10000
-- Frequency at which PNG images are written.
img_iter = 20

-- The pseudo-random number generator is initialized at a deterministic -- value, for proper validation of the output for the exercise series.  random.seed(1)
-- x- and y-pos are initialized to a square with side-length 2*ini_radius.
posx = map (\x -> x*2.*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 13) :: [Float]--random.random(numbodies) 
posy = map (\x -> x*2.*ini_radius + 0.5-ini_radius) $ take numbodies $ randoms (mkStdGen 13) :: [Float]--random.random(numbodies) *2.*ini_radius + 0.5-ini_radius
-- We only keep the bodies inside a circle of radius ini_radius.
copyCycle body = body{momentum = array([-r[1], r[0]]) * mass*inivel*norm(r)/ini_radius} where r = map (-) (pos body) [0.5,0.5]

bodies = map copyCycle [ _init mass px py | (px,py) <- zip posx posy, (px-0.5)^2 + (py-0.5)^2 < ini_radius^2 ]

main :: IO ()
main = someFunc
