#IMPORTS
from hw4_util import read_deaths
import sys

#DEFINITIONS
def trend(county):
    data = []   #NEW LISTS
    signs = []

    first = read_deaths(county)

    for n in range(len(first) - 1):
        change = first[n+1] - first[n]
        data.append(change) #ADD DATA TO NEW LIST
    
    for i in range(len(data)):
        if data[-1-i] < -5: # NEGATIVE
            signs.append('-')
        elif data[-1-i] > 5:
            signs.append('+')   #POSITIVE
        elif -5 < data[-1-i] < 5:
            signs.append('=')   #LESS THAN 5 EITHER WAY, SO EQUAL
                     
    print(county+':'"""
       2013  2004
Trend: {}
""".format("".join(signs)))
    return signs.count('+') - signs.count('-') # COUNT TO FIND STATISTICS

#INPUTS
first_county = input('Enter the first area to check => ')
print(first_county)

if read_deaths(first_county) == []: #INVALID NAME
    print(first_county, 'is an invalid name')
    sys.exit()
    
second_county = input('Enter the second area to check => ')
print(second_county)

if read_deaths(second_county) == []:    #INVALID NAME
    print(second_county, 'is an invalid name')
    sys.exit()

print()
#CREATE FOR BOOLEANS
trend1 = trend(first_county)
trend2 = trend(second_county)

#LOWER SCORE MEANS BETTER STATISTICS
#PRINT STATEMENTS
if trend1 > trend2: 
    print(second_county, 'has better trend statistics than', first_county + '.')
elif trend2 > trend1:
    print(first_county, 'has better trend statistics than', second_county + '.')
else:
    print(first_county, 'and', second_county, 'are the same')