#IMPORTS
import json
from Bird import *
from Pig import *
from Barrier import *

#MAIN
if __name__ == "__main__":
    
    filename = input('Enter the name of the data file => ')
    print(filename)
    
    #OPEN FILE, STRIP, AND LOAD
    f = open(filename.strip())
    data = f.read().strip()
    #MAKE DICTIONARY
    dictionary = json.loads(data)
    
    print()
    #NEW LIST
    birds = []
    for bird in dictionary['birds']:
        #ADD BIRD OBJECTS TO A LIST
        birds.append(Bird(bird['mass'], bird['y0'], bird['x0'], bird['dy'], bird['dx'], bird['name'], bird['radius']))
        
    #PRINT INITIAL INFORMATION ABOUT BIRDS
    print('There are {} birds:'.format(len(birds)))
    #FOR LOOP TO PRINT BIRDS
    for bird in birds:
        print('    {}: ({:.1f},{:.1f})'.format(bird.name, float(bird.x), float(bird.y)))
    
    #PIG LIST
    pigs = []
    for pig in dictionary['pigs']:
        #ADD PIG OBJECTS TO A LIST
        pigs.append(Pig(pig['xc'], pig['radius'], pig['name'], pig['yc']))
        
    #PRINT INITIAL INFORMATION ABOUT PIGS
    print()
    print('There are {} pigs:'.format(len(pigs)))
    #FOR LOOP TO PRINT PIGS
    for pig in pigs:
        print('    {}: ({:.1f},{:.1f})'.format(pig.name, float(pig.x), float(pig.y)))
    
    #BARRIER LIST
    barriers = []
    for barrier in dictionary['barriers']:
        #ADD BARRIER OBJECTS TO A LIST
        barriers.append(Barrier(barrier['yc'], barrier['name'], barrier['xc'], barrier['radius'], barrier['strength']))
    
    #PRINT INITIAL INFORMATION ABOUT BARRIERS
    print()
    print('There are {} barriers:'.format(len(barriers)))
    #FOR LOOP TO PRINT BARRIERS
    for barrier in barriers:
        print('    {}: ({:.1f},{:.1f})'.format(barrier.name, float(barrier.x), float(barrier.y)))        
    
    
    #INITIALIZE TIME COUNTER
    time = 0
    
    #KEEP TRACK OF WHAT BIRD YOURE ON
    bird_num = 0
    
    #EMPTY LINE BETWEEN INFO AND SIMULATION
    print()
    
    #MAKE SURE THERE ARE PIGS
    if len(pigs) == 0:
        print('Time {}: All pigs are popped. The birds win!'.format(time))   
    
    #PRINT OUT A MESSAGE WHEN YOU ADD THE FIRST BIRD
    else:
        print('Time {}: {} starts at ({:.1f},{:.1f})'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y)))    
    
        #INFINITE LOOP, BREAKS WHEN NO BIRDS OR PIGS
        while(True):
            
            #INCREMENT THE TIME COUNTER    
            time += 1
            
            #MOVE THE BIRD THE NECESSARY AMOUNT
            birds[bird_num].fly()
            
            #CHECK FOR COLLISIONS 
            
            #BEGIN WITH PIGS
            for pig in pigs:
                #CHECK THE COLLISION
                if pig.distance(birds[bird_num]) <= (birds[bird_num].radius + pig.radius):
                    
                    #PRINT OUT COLLISION MESSAGE
                    print('Time {}: {} at ({:.1f},{:.1f}) pops {}'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y), pig.name))
                    #print('Pig {} was popped at ({},{}) with radius {}'.format(pig.name, pig.x, pig.y, pig.radius))
                    #POP THE PIG
                    pigs.remove(pig)
                    #ADJUST SPEED
                    birds[bird_num].pig_collide()
                    #PRINT UPDATED SPEED MESSAGE
                    print('Time {}: {} at ({:.1f},{:.1f}) has (dx, dy) = ({:.1f},{:.1f})'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y), float(birds[bird_num].dx), float(birds[bird_num].dy)))
                    #BREAK LOOP
                    break
                
            #CHECK FOR BARRIER COLLISION
            for barrier in barriers:
                #CHECK THE COLLISION
                if barrier.distance(birds[bird_num]) <= (birds[bird_num].radius + barrier.radius):
                    
                    #UPDATE BARRIER STRENGTH
                    barrier.collide(birds[bird_num].mass, birds[bird_num].speed())
                        
                    #PRINT OUT COLLISION MESSAGE
                    print('Time {}: {} at ({:.1f},{:.1f}) hits {}, Strength {:.1f}'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y), barrier.name, float(barrier.strength)))
                    
                    #REMOVE BARRIER IF ITS STRENGTH IS TOO LOW
                    if barrier.strength == 0:
                        print('Time {}: {} crumbles'.format(time, barrier.name))
                        barriers.remove(barrier)                    
                        
                    #UPDATE BIRD SPEED
                    birds[bird_num].barrier_collide()
                        
                    #PRINT UPDATED BIRD SPEED MESSAGE
                    print('Time {}: {} at ({:.1f},{:.1f}) has (dx, dy) = ({:.1f},{:.1f})'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y), float(birds[bird_num].dx), float(birds[bird_num].dy)))
                    #BREAK LOOP
                    break
                
            #CHECK FOR OTHER EVENTS
            
            #NO PIGS LEFT, ONLY BIRDS
            if len(pigs) == 0:
                print('Time {}: All pigs are popped. The birds win!'.format(time))
                break            
            
            #MAKE SURE SPEED ISNT TOO LOW
            if birds[bird_num].speed() < 6:
                print('Time {}: {} at ({:.1f},{:.1f}) with speed {:.1f} stops'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y), float(birds[bird_num].speed())))
                #NO MORE BIRDS
                if bird_num + 1 == len(birds):
                    #VICTORY MESSAGE
                    print('Time {}: No more birds. The pigs win!'.format(time))
                    print('Remaining pigs:')
                    #PRINT ALL PIGS LEFT IF THEY WIN
                    for pig in pigs:
                        print(pig.name)      
                    break
                    
                bird_num += 1
                #PRINT OUT A MESSAGE WHEN YOU ADD A NEW BIRD TO THE BOARD
                print('Time {}: {} starts at ({:.1f},{:.1f})'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y)))
                
                
            #MAKE SURE POSITION IS OKAY
            if birds[bird_num].x-birds[bird_num].radius < 0 or \
               birds[bird_num].x+birds[bird_num].radius > 1000 or \
               birds[bird_num].y-birds[bird_num].radius < 0 or \
               birds[bird_num].y+birds[bird_num].radius > 1000:
                   
                #OUT OF THE GAME   
                print('Time {}: {} at ({:.1f},{:.1f}) has left the game'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y)))
                
                
                #PIGS WIN, PRINT MESSAGE AND REMAINING PIGS
                if bird_num + 1 == len(birds):
                    print('Time {}: No more birds. The pigs win!'.format(time))
                    print('Remaining pigs:')
                    for pig in pigs:
                        print(pig.name)
                    break
                
                bird_num += 1
                #PRINT OUT A MESSAGE WHEN YOU ADD A NEW BIRD TO THE BOARD
                print('Time {}: {} starts at ({:.1f},{:.1f})'.format(time, birds[bird_num].name, float(birds[bird_num].x), float(birds[bird_num].y)))