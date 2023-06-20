import Test.QuickCheck
import NBodyProblem.BurnesAndHut
import System.IO.Unsafe
import System.IO
import System.Cmd
import System.Directory
import Data.IORef

tester1 [k,x,y] = do
 let node = _init k x y 
 let l1 = [m node,s node,(m_pos node) !! 0,(m_pos node) !! 1,(momentum node) !! 0,(momentum node) !! 1]
 nodeIO <- newIORef node
 resetTo0thQuadrant nodeIO
 intoNextQuadrant nodeIO
 system "python test.py " ++ show k ++ " " ++ show x ++ " " ++ show y
 answers <- readFile "file_test.txt"
 let ans = lines answers
 removeFile "file_test.txt"
 node1 <- readIORef nodeIO
 let l2 = [m node1,s node1,(m_pos node1) !! 0,(m_pos node1) !! 1,(momentum node1) !! 0,(momentum node1) !! 1]
 return ((map (read::Float) ans)==(l1++l2))

tester :: [Float] -> Bool
tester a = unsafePerformIO $ tester1 a

main :: IO ()
main = do--putStrLn "Test suite not yet implemented"
 quickCheck tester
 putStrLn "\x1b[32mACCEPTED!\x1b[37m"