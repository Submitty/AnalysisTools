#IMPORTS
import math
from Pig import *
from Barrier import *

#BEGINNING OF CLASS FOR BIRD
class Bird(object):
    def __init__(self, mass, y0, x0, dy, dx, name, radius):
        #REASSIGNING VARIABLES
        self.x = x0
        self.y = y0
        self.name = name
        self.mass = mass
        self.dx = dx
        self.dy = dy
        self.radius = radius
        
    def speed(self):
        #GET SPEED FOR BIRD
        return math.sqrt((self.dx)**2 + (self.dy)**2)
    
    def fly(self):
        #ADD SELF.DX TO SELF.X
        self.x += self.dx
        self.y += self.dy
        
    def pig_collide(self):
        #COLLISION CHANGE
        self.dx /= 2
        
    def barrier_collide(self):
        #BARRIER GONE
        self.dx = 0
        self.dy = 0