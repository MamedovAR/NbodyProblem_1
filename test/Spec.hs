import Test.QuickCheck
import NBodyProblem.BurnesAndHut
import System.IO.Unsafe
import System.IO
import System.Cmd
import System.Directory
import Data.IORef

reader :: String -> Float
reader x = read x

tester1 [k,x,y] = do
    let node = _init k x y 
    let l1 = [m node,s node,(m_pos node) !! 0,(m_pos node) !! 1,(momentum node) !! 0,(momentum node) !! 1]
    nodeIO <- newIORef node
    resetTo0thQuadrant nodeIO
    intoNextQuadrant nodeIO
    createDirectoryIfMissing False ".nbodyproblem"
    writeFile ".nbodyproblem/test.py" $ "import os\n" ++
        "import sys\n" ++
        "from copy import deepcopy\n" ++
        "from numpy import array\n" ++
        "from numpy.linalg import norm\n" ++

        "class Node:\n" ++

        "    def __init__(self, m, x, y):\n" ++
        "        self.m = m\n" ++
        "        self.s = 1.0\n" ++
        "        self.m_pos = m * array([x, y])\n" ++
        "        self.momentum = array([0., 0.])\n" ++
        "        self.child = None\n" ++

        "    def into_next_quadrant(self):\n" ++
        "        self.s = 0.5 * self.s   # s: side-length of current quadrant.\n" ++
        "        return self._subdivide(1) + 2*self._subdivide(0)\n" ++

        "    def pos(self):\n" ++
        "        return self.m_pos / self.m\n" ++

        "    def reset_to_0th_quadrant(self):\n" ++
        "        self.s = 1.0\n" ++
        "        self.relpos = self.pos().copy()\n" ++

        "    def dist(self, other):\n" ++
        "        return norm(other.pos() - self.pos())\n" ++

        "    def force_on(self, other):\n" ++
        "        cutoff_dist = 0.002\n" ++
        "        d = self.dist(other)\n" ++
        "        if d < cutoff_dist:\n" ++
        "            return array([0., 0.])\n" ++
        "        else:\n" ++
        "            return (self.pos() - other.pos()) * (self.m*other.m / d**3)\n" ++

        "    def _subdivide(self, i):\n" ++
        "        self.relpos[i] *= 2.0\n" ++
        "        if self.relpos[i] < 1.0:\n" ++
        "            quadrant = 0\n" ++
        "        else:\n" ++
        "            quadrant = 1\n" ++
        "            self.relpos[i] -= 1.0\n" ++
        "        return quadrant\n" ++

        "node = Node(float(sys.argv[1]),float(sys.argv[2]),float(sys.argv[3]))\n" ++
        "os.system(\"pwd\")\n" ++
        "f=open(\".nbodyproblem/file_test.txt\",\"w\")\n" ++
        "f.write(str(node.m)+'\\n'+str(node.s)+\"\\n\"+str(node.m_pos[0])+\"\\n\"+str(node.m_pos[1])+\"\\n\"+str(node.momentum[0])+\"\\n\"+str(node.momentum[1])+\"\\n\")\n" ++
        "node.reset_to_0th_quadrant()\n" ++
        "node.into_next_quadrant()\n" ++
        "f.write(str(node.m)+'\\n'+str(node.s)+'\\n'+str(node.m_pos[0])+'\\n'+str(node.m_pos[1])+'\\n'+str(node.momentum[0])+'\\n'+str(node.momentum[1]))\n"
    system ("python .nbodyproblem/test.py " ++ show k ++ " " ++ show x ++ " " ++ show y)
    answers <- readFile "file_test.txt"
    let ans = lines answers
    removeDirectoryRecursive ".nbodyproblem"
    node1 <- readIORef nodeIO
    let l2 = [m node1,s node1,(m_pos node1) !! 0,(m_pos node1) !! 1,(momentum node1) !! 0,(momentum node1) !! 1]
    return ((map reader ans)==(l1++l2))

tester :: [Float] -> Bool
tester a = unsafePerformIO $ tester1 a

main :: IO ()
main = do--putStrLn "Test suite not yet implemented"
    quickCheck (\a -> tester [a,0.5,0.5])
    -- putStrLn "\x1b[32mACCEPTED!\x1b[37m"