#IMPORTS
import json
from Deck import *

#INPUT FILE
filename = input('What file do you want to use? ')
print(filename)
#OPEN WITH CORRECT ENCODING
f = open(filename, encoding="utf8")
#MAKE DICTIONARY WITH EACH LINE
line = f.readline()
cards = json.loads(line)
#MAKE COPY
cards_copy = cards.copy()
#EASIER TO REMOVE CARDS FROM
cards2 = list(cards.keys())

#NEW DICTIONARY, EMPTY
empty = dict()
#DECK OBJECT IS EMPTY DICTIONARY
deck = Deck(empty)
#ORIGINAL DECK HAS ALL CARDS VIA COPY
original_deck = Deck(cards_copy)

#NEW LISTS
different = []
attributes = []
#ADD DICTIONARIES TO LIST
for card in cards:
    new_dict = dict(cards[card])
    attributes.append(new_dict)

#NEW DICTIONARIES
tracking = dict()
types = dict()
#KEEP TRACK OF NUMBER OF CARDS WITH ATTRIBUTE
for item in attributes:
    for thing in item.keys():
        if thing in tracking:
            #IF ITS THERE ALREADY, INCREASE COUNT
            tracking[thing] += 1
        else:
            #IF NOT, INITIALIZE COUNT
            tracking[thing] = 1
            #TYPE
            types[thing] = type(item[thing])
            
print("The card catalog has",len(attributes),"different cards and",len(tracking.keys()),"total attributes")
#PRINT EACH ATTRIBUTES SENTENCE
for item in sorted(tracking.keys()):
    print('    ' + item,"occurs in",tracking[item],"cards and has type",types[item])
    
    
#MAIN CODE    
if __name__ == '__main__':
    
    #WHILE THERE ARE ACTIVE CARDS
    while True:
        print()
        #AMOUNT OF ACTIVE CARDS
        print('There are {} cards in the active list'.format(len(cards2)))
        
        #CHOICE OF NEXT ACTION
        choice = input('(A)dd, (F)ilter, (E)nd, (L)ist, (P)rint: ')
        print(choice)
        
        #MAKE UPPER TO ACCOMODATE
        choice = (choice.upper())
        
        #IF A, ADD USING CLASS
        if choice == 'A':
            for card in sorted(cards2): #SORT AND GO THROUGH ALL
                deck.add_card(cards[card])  #USE CLASS TO ADD TO DECK
            cards2 = cards.keys()   #CARDS2 EQUALS KEYS
                
        #IF E, END USING BREAK        
        elif choice == 'E':
            break
        
        #IF P, USE STR FUNCTION FROM CLASS
        elif choice == 'P':
            print(deck.__str__())
            
            
        #IF L, PRINT LIST    
        elif choice == 'L':
            for card in sorted(cards2):
                print(card)
                
        #IF F        
        elif choice == 'F':
            while True:
                #GIVE OPTIONS
                print('1: manaCost\n2: name\n3: power\n4: text\n5: toughness\n6: type')
                #WHAT USER WANTS TO FILTER
                filter_choice = int(input('Select an attribute to filter on (1-6) or 0 to return to main menu: '))
                print(filter_choice)
                
                #IF NOT AN ALLOWED CHOICE
                if filter_choice < 0 or filter_choice > 6:
                    continue
                
                #IF 0, BACK TO MAIN (BREAK)
                elif filter_choice == 0:
                    break
                
                #IF ALLOWED NUMBERS
                else:
                    #INPUT WHAT TO FILTER BY
                    filter_string = input('Enter filter string: ')
                    print(filter_string)                    
                    
                    #NEW LIST
                    new_cards = []
                    
                    #IF MANACOST
                    if filter_choice == 1:
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'manaCost' in cards[card].keys():    #MUST HAVE MANACOST
                                if filter_string in cards[card]['manaCost']:    #IF STRING IS IN MANACOST, APPEND
                                    new_cards.append(card)
                                    
                    #IF NAME    
                    elif filter_choice == 2:  
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'name' in cards[card].keys():    #MUST HAVE NAME
                                if filter_string in cards[card]['name']:    #IF STRING IS IN NAME, APPEND
                                    new_cards.append(card)      
                    
                    #IF POWER    
                    elif filter_choice == 3:
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'power' in cards[card].keys():   #MUST HAVE POWER
                                if filter_string in cards[card]['power']:   #IF STRING IS IN POWER, APPEND
                                    new_cards.append(card)                       
                    
                    #IF TEXT        
                    elif filter_choice == 4:
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'text' in cards[card].keys():    #MUST HAVE TEXT
                                if filter_string in cards[card]['text']:    #IF STRING IS IN TEXT, APPEND
                                    new_cards.append(card)                        
                    
                    #IF TOUGHNESS    
                    elif filter_choice == 5:
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'toughness' in cards[card].keys():   #MUST HAVE TOUGHNESS
                                if filter_string in cards[card]['toughness']:   #IF STRING IS IN TOUGHNESS, APPEND
                                    new_cards.append(card)                            
                    
                    #IF TYPE
                    elif filter_choice == 6:
                        for card in cards2: #GO THROUGH ALL CARDS
                            if 'type' in cards[card].keys():    #MUST HAVE TYPE
                                if filter_string in cards[card]['type']:    #IF STRING IS IN TYPE, APPEND
                                    new_cards.append(card)
                
                #FOR COUNT, RESET CARDS2 TO EQUAL NEW_CARDS                    
                cards2 = new_cards          
                #BREAK
                break
            
        #IF BAD INPUT, CONTINUE
        else:
            continue