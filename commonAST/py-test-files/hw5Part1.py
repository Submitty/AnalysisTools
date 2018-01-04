#MUST IMPORT TO USE FUNCTION
import random  

#DEFINITION TO GET VALUES
def move_trainer():
    directions = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']   #LIST OF DIRECTIONS
    random1 = random.choice(directions) #CHOOSES FROM LIST
    random2 = random.random()   #CHOOSES A VALUE FROM 0 TO 1
    print('Direction', random1 +',', 'value {:.2f}'.format(random2))    #TWO DECIMAL PLACES FOR VALUE
    
#INPUTS    
M = int(input('Enter the integer number of rows => '))
print(M)
N = int(input('Enter the integer number of cols => '))
print(N)

random.seed(10 * M + N)

n = 0
while n < 15:   #KEEPS GOING UNTIL 15 TIMES
    (move_trainer())
    n += 1  #ALLOWS THE LOOP TO END 
