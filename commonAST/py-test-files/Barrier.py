#IMPORTS
from Bird import *
import math

#BEGINNING OF CLASS FOR BARRIERS
class Barrier(object):
    def __init__(self, yc, name, xc, radius, strength):
        #RE ASSIGN VARIABLES
        self.name = name
        self.radius = radius
        self.strength = strength
        self.x = xc
        self.y = yc
        
    def collide(self, mass, speed):
        #IF COLLIDING, CHANGE STRENGTH
        damage = self.strength - (mass * (speed**2))
        if damage >= 0:
            self.strength = damage
        else:
            self.strength = 0
            
    def distance(self, bird):
        #CHANGE DISTANCE
        return math.sqrt((self.x-bird.x)**2+(self.y-bird.y)**2)