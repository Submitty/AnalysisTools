#IMPORT
import random
#FUNCTIONS
def move_trainer(position, bounds, prob):   #SAME FROM PART 2
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

def run_one_simulation(grid, p):    #P IS PROBABILITY INPUT
    M = len(grid)   #ROW
    N = len(grid[0])    #COLUMN
    #ORIGINIAL POSITION
    position = (M//2, N//2)
    #CANT GO OUT OF BOUNDS
    bounds = (M-1, N-1)
    #START WITH NO POKEMON
    pokemon_caught = 0
    #250 TIME STEPS EACH SIMULATION
    for n in range(0, 250):
        position, pokemon_was_caught = move_trainer(position, bounds, p)    #MAKE A TUPLE
        if pokemon_was_caught:
            pokemon_caught += 1
        grid[position[0]][position[1]] += 1 #CHANGE POSITION EVEN IF POKEMON WAS NOT CAUGHT
    return grid, pokemon_caught        

def run_simulations(row, cols, sims):   #MUST HAVE SIMS AS ARGUMENT
    #START ALL AT 0 TO DEFINE
    max_pokemon = 0
    min_pokemon = 0
    min_pokemon_simulation = 0
    max_pokemon_simulation = 0
    pokemon_caught_total = 0
    #NEW LIST
    grid = []
    for n in range(0, row):
        row = []    #NEW LIST
        for n in range(0, cols):
            row.append(0)   #ADD TO ROW LIST
        grid.append(row)    #APPENDING TO GRID (EMPTY LIST)
    grid[len(grid)//2][len(grid[0])//2] += sims   
    # FOR ALL SIMULATIONS REQUESTED
    for n in range(0, sims):
        grid, pokemon_caught = run_one_simulation(grid, p)  #IDENTIFY GRID AND POKEMON CAUGHT
        pokemon_caught_total += pokemon_caught  
        #IF IT IS THE FIRST SIMULATION
        if n == 1:
            #DEFINE VARIABLES FOR OTHER SIMULATION USE
            #ONLY HAVE ONE AMOUNT OF POKEMON TO CAPTURE IN ONE SIMULATION
            max_pokemon = pokemon_caught   
            min_pokemon = pokemon_caught
            max_pokemon_simulation = 1
            min_pokemon_simulation = 1
        else:   
            if pokemon_caught > max_pokemon:    #IF NEXT POKEMON CAUGHT IS GREATER THAN THE LAST MAX, REPLACE IT
                max_pokemon = pokemon_caught
                max_pokemon_simulation = n + 1  #ADD ONE TO SIMULATION COUNT
            if pokemon_caught < min_pokemon:    #IF NEXT POKEMON IS LESS THAN THE CLAIMED MIN, REPLACE IT
                min_pokemon = pokemon_caught
                min_pokemon_simulation = n + 1  #ADD ONE TO SIMULATION COUNT
            
    return pokemon_caught_total, grid, max_pokemon, min_pokemon, max_pokemon_simulation, min_pokemon_simulation     #ALL IMPORTANT VALUES TO BE RETURNED

#TO PRINT THE GRID
def print_the_grid(grid, rows, cols):
    for n in range(rows):   #FOR ALL ROWS
        for m in range(cols):   #FOR ALL COLS
            print('{:5d}'.format(grid[n][m]), end = '') #PRINT THE NUMBER AT THAT LOCATION
        print()    #PRINT A BLANK LINE

#LIST OF DIRECTIONS
directions = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']

#IMPUTS
rows = int(input('Enter the integer number of rows => '))
print(rows)
cols = int(input('Enter the integer number of cols => '))
print(cols)
p = float(input('Enter the probability of finding a pokemon (<= 1.0) => '))
print(p)
sims = int(input('Enter the number of simulations to run => '))
print(sims)
print() #NEED A BLANK LINE

#RANDOM SETTING SEEDS 
seed_value = 10*rows + cols
random.seed(seed_value) 

#GIVE RUN_SIMULATIONS A VARIABLE TO MAKE FUTURE LESS CONFUSING
info = run_simulations(rows, cols, sims)
#DEFINE GRID VIA INFO 
grid = info[1]
#GRID FIRST LOCATION
minimum = grid[0][0]
maximum = grid[0][1]

#FOR ALL ROWS
for i in range(0, rows):
    #FOR ALL COLUMNS
    for j in range(0, cols):
        if maximum < grid[i][j]:    #IF LESS THAN PREVIOUS LOCATION, RENAME MAX
            maximum = grid[i][j]
        if minimum > grid[i][j]:   #IF MORE THAN PREVIOUS LOCATION, RENAME MIN
            minimum = grid[i][j]

#ALL FINAL PRINT STATEMENTS
print_the_grid(info[1], rows, cols) 
print()
print('Total pokemon caught is', info[0])
print('Minimum pokemon caught was', info[3], 'in simulation', info[5])
print('Maximum pokemon caught was', info[2], 'in simulation', info[4])
print('The least visited space was visited', minimum, 'times')
print('The most visited space was visited', maximum, 'times')