#OPEN FILE
file = open('DrWhoVillains.tsv', encoding='utf8')
#NEW LIST
new_villains = [] 
#ADDS NAMES, AMOUNT OF STORIES AND STORIES TO NEW LIST
for line in file:
    stories= []
    villains = line.strip().split('\t') #SPLIT BY TAB, STRIP WHITE SPACE
    temp_stories = villains[7].split(',')   #STORIES SPLIT BY COMMAS
    for i in range(len(temp_stories)):
        stories.append(temp_stories[i].strip().lower()) #MUST STRIP WHITE SPACE, MAKE LOWER SO NO DUPLICATES
    stories = set(stories)  #MAKE A SET
    info = [len(stories), villains[0], stories]
    if info[1] != "Villain":    #RID EXAMPLE VILLAIN
        new_villains.append(info)

#SORTS NEW LIST
new_villains.sort(reverse=True) 
    
#INPUTS
count = 0   #INITIATE COUNT AT 0
start_num = int(input('Which villain is the start (1 to {}): '.format(int(len(new_villains)) )))
print(start_num)
target_num = int(input('Which villain is the target (1 to {}): '.format(int(len(new_villains)) )))
print(target_num)

#MUST SUBTRACT ONE TO GET ACTUAL VILLAIN
start_num -= 1
target_num -= 1

cmd = ''
steps = 0   #INITIATE STEPS

#GETS INFO FOR VILLAIN WANTED
start_villain = new_villains[start_num] 
target_villain = new_villains[target_num]

#[1] GIVES NAME AND [0] AMOUNT OF STORIES
print()
print('Target villain is', target_villain[1], 'with', target_villain[0], 'stories.')

while(cmd != "end"):
    
    #STATING CURRENT VILLAIN
    print()
    print('Current villain is', start_villain[1].strip(), 'with', start_villain[0], 'stories.')
    
    #REACHING TARGET VILLAIN
    if start_villain[1] == target_villain[1]:
        print('You have reached the target villain after', steps, 'steps.')
        break
    #NEW LIST FOR VILLAINS THAT HAVE SIMILAR STORIES TO THE START VILLAIN    
    similar = []
    
    #GET NEW VILLAINS FOR NEW LIST
    for i in range(len(new_villains)):
        if new_villains[i][1] != start_villain[1]:  #NOT SAME VILLAIN
            if len(new_villains[i][2] & start_villain[2]) > 0:  #SIMILAR STORIES GREATER THAN 0
                similar.append((new_villains[i][0], new_villains[i][1], new_villains[i][2]))
                    
    similar.sort(reverse = True)    #SORTING
    
    #IF NO SIMILAR STORIES, PRINT MESSAGE, END LOOP
    if len(similar) == 0:
        print('\nYou have reached a dead end at {} after {} steps.'.format(start_villain[1], steps))
        break
    
    print()
    print("Available candidates:")
    count = 1
    #PRINTS LIST OF VILLAINS WITH SIMILAR STORIES
    for villain in similar:
        print("{:3d}: {}".format(count, (villain[1]).strip()))
        count+=1    #INCREASE COUNT
    
    #NEW INPUT    
    cmd = input("Which villain is the new start (1 to {}) or 'end' to quit: ".format(len(similar))).strip()
    print(cmd)

    #IF END, PRINT MESSAGE, BREAK
    if cmd == 'end':
        print('\nYou quit after', steps, 'steps without reaching the {}.'.format(target_villain[1]))
        break
    
    #TRY AGAIN IF THEY ENTER 0
    if cmd == '0':
        continue
    
    #IF NUMBER IS TOO LARGE, TRY AGAIN
    if int(cmd) > len(similar):
        continue
    
    #REASSIGN START_VILLAIN 
    start_villain = similar[int(cmd)-1]
        
    steps += 1 #INCREASE STEPS
    
