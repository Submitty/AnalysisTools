#IMPORTS
import hw4_util
import sys

#DEFINITIONS
def print_pokemon():
    print('Current pokemon:')
    for n in range(len(pokemon)):
        if pokemon[n] not in pokemon_captured:
            print (' ' * 5 + pokemon[n], 'at', locations[n])  #ALL POKEMON THAT HAVENT BEEN CAPTURED
#LISTS
pokemon_captured = []   #NEW LIST TO MOVE TO
pokemon, locations = hw4_util.read_pokemon()    #TWO SEPERATE LISTS
commands = ['N','S', 'E', 'W', 'END', 'PRINT']

turn_number = 0 #START AT 0TH TERM
n = 0
print('Current pokemon:')   #ORIGINAL PRINT
while n <= (len(pokemon) - 1):
    print(' ' * 5 + pokemon[n], 'at', locations[n])
    n += 1

print() #NEED A BLANK LINE
while len(pokemon) > 0: #WHILE NOT ALL POKEMON HAVE BEEN REMOVED
    command = input("N,S,E,W to move, 'print' to list, or 'end' to stop ==> ")
    print(command)
    command = command.upper() #SO PROGRAM ACCEPTS ALL COMMANDS
    if not command in commands:
        turn_number += 1    #STILL COUNTS AS A TURN
        continue
    if command == 'END':
        print()
        print('Simulation ended.')
        turn_number += 1    #COUNTS AS A TURN
        sys.exit()    #EXITS PROGRAM
    if command == 'PRINT':  #PRINT LIST OF POKEMON YET TO BE CAPTURED
        print_pokemon()
        print()
        turn_number += 1
        #NESTED IF STATEMENTS
    if command in commands and command != 'PRINT':  #EVERY COMMAND BUT PRINT
        pokemon_moving = input('Which pokemon moved ' + command + '? ')
        print(pokemon_moving)
        number = int(pokemon.index(pokemon_moving)) #FIND WHERE POKEMON IS ON THE LIST TO FIND LOCATION AS WELL    
        #LOCATION CHANGES BASED ON DIRECTION, ALL COUNT AS TURNS
        if command == 'N':
            locations[number] = (locations[number][0], locations[number][1] - 1)
            print(pokemon[number], 'moved to location', locations[number])     
            turn_number += 1
        if command == 'S':
            locations[number] = (locations[number][0], locations[number][1] + 1)
            print(pokemon[number], 'moved to location', locations[number])
            turn_number += 1
        if command == 'E':
            locations[number] = (locations[number][0] + 1, locations[number][1])  
            print(pokemon[number], 'moved to location', locations[number])
            turn_number += 1
        if command == 'W':
            locations[number] = (locations[number][0] - 1, locations[number][1])
            print(pokemon[number], 'moved to location', locations[number])
            turn_number += 1   
            #IF IT REACHES THE ORIGIN, IT IS CAPTURED
            if locations[number][0] == 0 and locations[number][1] == 0:
                print('You capture a', pokemon[number], 'on turn', turn_number)
                pokemon.pop(number)  #REMOVE POKEMON NAME FROM LIST
                locations.pop(number)   #REMOVES LOCATION FROM LIST
   
    
  