directions = [ 'N' , 'NE' , 'E' , 'SE' , 'S ' , 'SW' , 'W' , 'NW' ]
attacks = ['Walk', 'Run', 'Turn', 'Attack'] #TYPES OF ATTACKS
beginning = (75,75) #BEGINNING COORDINATES
direction = 'E' #BEGINNING DIRECTION
print('Pikachu at', beginning, 'facing E')

x = 0

input_attacks = []

while x < 5:    #ONLY ASKS FOR FIVE INPUTS
    attack = input("What does Pikachu do ('Walk','Run','Turn','Attack')? ")
    print(attack)
    input_attacks.append(attack)    #ADDS TO LIST
    attack = attack.lower().capitalize()    #MAKE ALL INPUTS HAVE FIRST LETTER CAPITALIZED AND REST LOWER CASE
    x +=1
    if attack not in attacks:
        print('Pikachu at', beginning, 'facing', direction)
        
    # WALK ACTION, MATH DEPENDS ON DIRECTION
    if attack == 'Walk':
        if direction == 'N':
            beginning = (beginning[0], beginning[1] - 20)   #REDEFINING COORDINATES
        elif direction == 'S':
            beginning = (beginning[0], beginning[1] + 20)
        elif direction == 'E':
            beginning = (beginning[0] + 20, beginning[1])
        elif direction == 'W':
            beginning = (beginning[0] - 20, beginning[1])
        elif direction == 'SE':
            beginning = (beginning[0] + 20, beginning[1] + 20)
        elif direction == 'SW':
            beginning = (beginning[0] - 20, beginning[1] + 20)
        elif direction == 'NE':
            beginning = (beginning[0] + 20, beginning[1] - 20)
        elif direction == 'NW':
            beginning = (beginning[0] - 20, beginning[1] - 20)
        if beginning[0] < 0:
            beginning1 = (0, beginning[1])
            beginning = beginning1                
        print('Pikachu at', beginning, 'facing', direction)  
        
    # TURN ACTION, CHANGES 45 DEGREES, COORDINATES DON'T CHANGE
    if attack == 'Turn':
        if direction == 'N':
            direction = 'NE'    #REDEFINING DIRECTION
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'NE':
            direction = 'E'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'E':
            direction = 'SE'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'SE':
            direction = 'S'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'S':
            direction = 'SW'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'SW':
            direction = 'W'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'W':
            direction = 'NW'
            print('Pikachu at', beginning, 'facing', direction)
        elif direction == 'NW':
            direction = 'N'
            print('Pikachu at', beginning, 'facing', direction)
            
    # RUN ACTION, ADDS OR SUBTRACTS 50 STEPS BASED ON DIRECTION        
    if attack == 'Run': #BOTH VARIABLES ARE CHANGED
        if direction == 'N':
            direction = 'S'
            beginning = (beginning[0], beginning[1] + 50)
        elif direction == 'NE':
            direction = 'SW'
            beginning = (beginning[0] - 50, beginning[1] + 50)
        elif direction == 'E':
            direction = 'W'
            beginning = (beginning[0] - 50, beginning[1])
        elif direction == 'SE':
            direction = 'NW'
            beginning = (beginning[0] - 50, beginning[1] - 50)
        elif direction == 'S':
            direction = 'N'
            beginning = (beginning[0], beginning[1] - 50)
        elif direction == 'SW':
            direction = 'NE'
            beginning = (beginning[0] + 50, beginning[1] - 50)
        elif direction == 'W':
            direction = 'E'
            beginning = (beginning[0] + 50, beginning[1])
        elif direction == 'NW':
            direction = 'SE'
            beginning = (beginning[0] + 50, beginning[1] + 50)
        if beginning[0] < 0:
            beginning1 = (0, beginning[1])
            beginning = beginning1            
        print('Pikachu at', beginning, 'facing', direction)           
            
# PRINT INPUT LIST AND SORTED INPUT LIST     
print()
print('All commands entered:', input_attacks)
input_attacks.sort()
print('Sorted commands:', input_attacks)