import read_names
import sys
read_names.read_from_file("top_names_1880_to_2014.txt")

#FINDING NAME'S INDEX 
def index(year):
    #IF NOT WITHIN 1880 AND 2014, PRINT NOTHING
        (female_names, female_counts) = read_names.top_in_year(year, 'f')
        (male_names, male_counts) = read_names.top_in_year(year, 'M')
        if year > 2014 or year < 1880:
            return -1
        #TEST TO SEE IF NAME IS IN TOP 250
        elif name not in female_names:
            print(name, 'is not among the top 250 names for', year)
        else:   #ASSIGNING VARIABLES
            rank = int(female_names.index(name))  
            male_name = male_names[rank]    
            female_count = female_counts[rank] 
            male_count = male_counts[rank]
            female_percent_top = (female_count/female_counts[0])*100
            male_percent_top = (male_count/male_counts[0])*100
            female_percent_sum = (female_count/sum(female_counts))*100
            male_percent_sum = (male_count/sum(male_counts))*100
            #PRINT STATEMENTS
            print('Year:', year)
            print('{:12s}: {:3d} {:5d} {:7.3f} {:7.3f}%'.format(name, (rank+1), female_count, female_percent_top, female_percent_sum))
            print('{:12s}: {:3d} {:5d} {:7.3f} {:7.3f}%'.format(male_name, (rank+1), male_count, male_percent_top, male_percent_sum))  
            print()

#INPUTS ENNTERED BY USER
year_input = input('Enter a year to begin our exploration: ')
year = int(year_input)
print(year)
#IF STATEMENT SO YEAR IS IN RANGE
if year < 1880 or year > 2014:
    print(year, 'does not lie between 1880 and 2014')
    sys.exit()  
    #LEAVES ERROR BUT TIS OK
else:   
    name = input('Enter a female name: ')
    print(name) #PRINT BLANK LINE
       
      
#NO PRINT BECAUSE PRINT IS IN FUNCTION
#FINAL RESULTS
print()
index(year-10)
index(year-5)
index(year)
index(year+5)
index(year+10)