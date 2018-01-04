#imports
import math
#functions
def volume_cone(radius, height):
    r = float(radius)   #radius cannot be a string in the math equation
    h = float(height)   #height cannot be a string in the math equation
    volume_oxygen = math.pi * (r**2) * (h/3)
    return volume_oxygen * (.21)*(.41)

def volume_cylinder(radius, height):
    r = float(radius)   #radius must be float to be in equation
    h = float(height)   #height must be float to be in equation
    volume = math.pi * (r**2) * h
    return volume * 210

#prints
radius_capsule = input('Radius of capsule (m) ==> ')
print(radius_capsule)
height_capsule = input('Height of capsule (m) ==> ')
print(height_capsule)
radius_reservoir = input('Radius of oxygen reservoir (m) ==> ')
print(radius_reservoir)
height_reservoir = input('Height of oxygen reservoir (m) ==> ')
print(height_reservoir)
reservoirs = input('Number of oxygen reservoirs available ==> ')
print(reservoirs)
reservoirs1 = int(reservoirs) #has to be an int for multiplication later on
print() #print a blank line
print('Oxygen needed per day is {:.3f}m^3.'.format(volume_cone(radius_capsule,height_capsule)))
print('Each cylinder holds {:.3f}m^3 at 3000 psi.'.format(volume_cylinder(radius_reservoir, height_reservoir)))
days = ((reservoirs1 * (volume_cylinder(radius_reservoir, height_reservoir)))/(volume_cone(radius_capsule, height_capsule)))
total_days = math.floor(days)   #floor rounds number of days down
print('Given', reservoirs, 'reservoirs, the trip can last', total_days, 'days.')