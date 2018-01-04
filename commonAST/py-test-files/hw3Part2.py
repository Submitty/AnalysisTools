#IMPORT LIST AND FUNCTION
import hw3_util
legos = hw3_util.read_legos('legos.txt')

#CREATED VARIABLES FOR AMOUNT OF CERTAIN LEGOS
onebyone = legos.count('1x1')
twobyone = legos.count('2x1')
twobytwo = legos.count('2x2')
twobyfour = legos.count('2x4')

#USER IMPUT
size = input('What type of lego do you need? ')
print(size)

number_of_twobyone = twobyone + onebyone//2 #NEED ONE TWO BY ONE AND TWO ONE BY ONES TO MAKE A TWO BY ONE
number_of_twobytwo = twobytwo + twobyone//2 + onebyone//4   #NEED FOUR ONE BY ONES, NEED TWO TWO BY ONES, AND ONE TWO BY TWO TO MAKE A TWO BY TWO
number_of_twobyfour = twobyfour + twobytwo//2 + twobyone//4 + onebyone//8   #NEED ONE TWO BY FOUR, TWO TWO BY TWOS, FOUR TWO BY ONES, AND EIGHT ONE BY ONES TO MAKE A TWO BY FOUR

#HARD CODED    
#IF STATEMENT FOR ONE BY ONE    
if size == '1x1':
    print()
    print('I can make {:d} {:s} pieces:'.format(onebyone, size))    # NEED ONE ONE BY ONE TO MAKE A ONE BY ONE
    print('-- {:d} pieces using 2x4 pieces.'.format(0))
    print('-- {:d} pieces using 2x2 pieces.'.format(0))
    print('-- {:d} pieces using 2x1 pieces.'.format(0))
    print('-- {:d} pieces using 1x1 pieces.'.format(onebyone))

#IF TWO BY ONE
elif size == '2x1':
    print()
    print('I can make {:d} {:s} pieces:'.format(number_of_twobyone, size))
    print('-- {:d} pieces using 2x4 pieces.'.format(0))
    print('-- {:d} pieces using 2x2 pieces.'.format(0))
    print('-- {:d} pieces using 2x1 pieces.'.format(twobyone))
    print('-- {:d} pieces using 1x1 pieces.'.format(onebyone//2))   #TAKES TWO ONE BY ONES TO MAKE A TWO BY ONE
    
#IF TWO BY TWO    
elif size == '2x2':
    print()
    print('I can make {:d} {:s} pieces:'.format(number_of_twobytwo, size))
    print('-- {:d} pieces using 2x4 pieces.'.format(0))
    print('-- {:d} pieces using 2x2 pieces.'.format(twobytwo))
    print('-- {:d} pieces using 2x1 pieces.'.format(twobyone//2)) #NEED TWO SO DIVIDE BY TWO
    print('-- {:d} pieces using 1x1 pieces.'.format(onebyone//4))  #NEED FOUR SO DIVIDE BY FOUR  

#IF TWO BY FOUR
elif size == '2x4':
    print()
    print('I can make {:d} {:s} pieces:'.format(number_of_twobyfour, size)) 
    print('-- {:d} pieces using 2x4 pieces.'.format(twobyfour)) #ONLY PIECE NEEDED
    print('-- {:d} pieces using 2x2 pieces.'.format(twobytwo//2)) #NEED TWO SO DIVIDE BY TWO
    print('-- {:d} pieces using 2x1 pieces.'.format(twobyone//4)) #NEED FOUR SO DIVIDE BY FOUR
    print('-- {:d} pieces using 1x1 pieces.'.format(onebyone//8)) # NEED EIGHT SO DIVIDE BY EIGHT
    
#IF NONE OF THE PIECES ARE THIS SIZE, IT IS ILLEGAL LEGO
else:
    print()
    print('Illegal lego')    