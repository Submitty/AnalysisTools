#IMPORT
import math
from Bird import *

#CLASS FOR PIG
class Pig(object):
    def __init__(self, xc, radius, name, yc):
        #REASSIGNING VARIABLES
        self.name = name
        self.radius = radius
        self.x = xc
        self.y = yc
        
    def distance(self, bird):
        #DISTANCE FORMULA 
        return math.sqrt((self.x-bird.x)**2+(self.y-bird.y)**2)