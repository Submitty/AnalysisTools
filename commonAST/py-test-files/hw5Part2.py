#IMPORT
import random

#FUNCTION
def move_trainer( position, bounds, prob):
    row = position[0]   #TUPLE SPLIT
    col = position[1]
    prob = float(prob)
    nextDir = random.choice(directions)
    caught = random.random()
    #IF STATEMENTS FOR DIRECTIONS
    if nextDir == 'N':
        row = max(0, row - 1) 
    elif nextDir == 'NE':
        row = max(0, row-1)
        col = min(bounds[1], col+1)
    elif nextDir == 'E':
        col = min(bounds[1],col+1)
    elif nextDir == 'SE':
        row = min(bounds[0], row + 1)
        col = min(bounds[1], col+1)
    elif nextDir == 'S':
        row = min(bounds[0], row+1)
    elif nextDir == 'SW':
        row = min(bounds[0], row+1)
        col = max(0, col-1)
    elif nextDir == 'W':
        col = max(0, col-1)
    elif nextDir == 'NW':
        row = max(0, row-1)
        col = max(0, col-1)
        #IF RANDOM NUMBER IS LESS THAN INPUT, CAUGHT
    if caught < prob:
        pokemon_caught = True
        #IF NOT, FREE POKEMON
    else: 
        pokemon_caught = False    
    return (row, col), pokemon_caught

#LIST OF DIRECTIONS
directions = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']

#INPUTS
M = int(input('Enter the integer number of rows => '))
print(M)
N = int(input('Enter the integer number of cols => '))
print(N)
p = float(input('Enter the probability of finding a pokemon (<= 1.0) => '))
print(p)
print()
#ORIGINIAL POSITION
position = (M//2, N//2)
#CANT GO OUT OF BOUNDS
bounds = (M-1, N-1)
#RANDOM SETTING SEEDS 
seed_value = 10*M + N
random.seed(seed_value)            

#TO COUNT EACH ROUND POKEMON AND TOTAL CAUGHT
counter = 0
counter2 = 0
pokemon_caught = 0

for i in range(1, 251):
    position, pokemon_caught = move_trainer(position, bounds, p)
    #IF CAUGHT, INCREASE BOTH COUNTS
    if pokemon_caught == True:
        counter += 1
        counter2 += 1
        #INCREASE BY 20 AND PRINT EVERY 20 STEPS
    if i % 20 == 0:
        print('Time step', str(i) +':', 'position', position, 'pokemon caught since the last report', counter) 
        counter = 0 #RESET COUNT TO 0 FOR NEW LINE

#move_trainer(position, bounds, p)
#ONCE IT REACHES 250, PRINT FINAL
if i == 250:
    print('After 250 time steps the trainer ended at position', position, 'with', counter2, 'pokemon.') 