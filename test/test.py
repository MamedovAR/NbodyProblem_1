import os
import sys
from copy import deepcopy
from numpy import array
from numpy.linalg import norm

class Node:
# A node represents a body if it is an endnote (i.e. if node.child is None)
# or an abstract node of the quad-tree if it has child.

    def __init__(self, m, x, y):
    # The initializer creates a child-less node (an actual body).
        self.m = m
        # Instead of storing the position of a node, we store the mass times
        # position, m_pos. This makes it easier to update the center-of-mass.
        self.m_pos = m * array([x, y])
        self.momentum = array([0., 0.])
        self.child = None

    def into_next_quadrant(self):
    # Places node into next-level quadrant and returns the quadrant number.
        self.s = 0.5 * self.s   # s: side-length of current quadrant.
        return self._subdivide(1) + 2*self._subdivide(0)

    def pos(self):
    # Physical position of node, independent of currently active quadrant.
        return self.m_pos / self.m

    def reset_to_0th_quadrant(self):
    # Re-positions the node to the level-0 quadrant (full domain).
        # Side-length of the level-0 quadrant is 1.
        self.s = 1.0
        # Relative position inside the quadrant is equal to modulephysical position.
        self.relpos = self.pos().copy()

    def dist(self, other):
    # Distance between present node and another node.
        return norm(other.pos() - self.pos())

    def force_on(self, other):
    # Force which the present node is exerting on a given body.
        # To avoid numerical instabilities, introduce a short-distance cutoff.
        cutoff_dist = 0.002
        d = self.dist(other)
        if d < cutoff_dist:
            return array([0., 0.])
        else:
            # Gravitational force goes like 1/r**2.
            return (self.pos() - other.pos()) * (self.m*other.m / d**3)

    def _subdivide(self, i):
    # Places node into next-level quadrant along direction i and recomputes
    # the relative position relpos of the node inside this quadrant.
        self.relpos[i] *= 2.0
        if self.relpos[i] < 1.0:
            quadrant = 0
        else:
            quadrant = 1
            self.relpos[i] -= 1.0
        return quadrant

node = Node(float(sys.argv[1]),float(sys.argv[2]),float(sys.argv[3]))
f=open("~/.nbodyproblem/file_test.txt","w")
f.write(node.m,'\n',node.s,'\n',node.m_pos[0],'\n',node.m_pos[1],'\n',node.momentum[0],'\n',node.momentum[1],'\n')
node.reset_to_0th_quadrant()
node.into_next_quadrant()
f.write(node.m,'\n',node.s,'\n',node.m_pos[0],'\n',node.m_pos[1],'\n',node.momentum[0],'\n',node.momentum[1])
