'''
Author:  Chuck Stewart
Date:    February 24, 2016

Purpose: A module to read in the Social Security Administration's top
   250 baby names for each year from 1880 up to and including 2014.

Usage: 
   1. Import the module

   2. Call read_names.read_from_file(fn) where fn is the file
      containing the names.  This returns a boolean, which for the
      purposes of the homework can be safely ignored.

   3. (names, counts) = read_names.top_in_year(year, f_or_m) where
      year is an int and f_or_m is a single character to indicate
      whether female ('F' or 'f') names are requested or male ('M' or
      'm') names are requested.  This function returns a tuple of
      lists, where the first list is the names and the second list is
      the count of occurrences of that name.
'''

import sys

'''
The following are the global lists and int storing all of the
information read in.  All of this really should be in a Python class
object, but at this point in the semester we have not studied these.
'''
all_female_names = []
all_female_counts = []
all_male_names = []
all_male_counts = []
first_year = -1


def read_from_file( file_name ):
    '''
    Read from file_name.  The format is the year, followed by the top
    250 female names,followed by the top 250 male names.  This repeats
    for each year in order.  Here are the first four lines to show the
    form of each line 
       1880
       Mary,F,7065
       Anna,F,2604
       Emma,F,2003
    '''
    in_f = open(file_name,'r')

    '''  These are the counts for one year...  '''
    female_names = []
    female_counts = []
    male_names = []
    male_counts = []
    year = -1   # this is reset when the very first year is read in
    line_num = 0
    
    '''  Tell Python to use these outside variables '''
    global all_female_names
    global all_female_counts
    global all_male_names
    global all_male_counts
    global first_year

    '''  Handle one line of input at a time '''
    for line in in_f:
        line = line.strip().split(',')
        line_num += 1

        #  Handle the special case of the very first line and year
        if first_year == -1 and len(line) == 1 and line[0].isdigit():
            first_year = int(line[0])
            year = first_year

        #  Error check on the format of the first line
        elif first_year == -1:
            print("Error: initial format on line number", line_num)
            return False

        #  After the first line we'll end up here each time.  This
        #  line is to test for the start of the next year and will
        #  succeed only after 500 names have been read
        elif len(line) == 1 and line[0].isdigit(): 
            #  Add the names from the year just completely read to the
            #  end of the global lists of lists
            all_female_names.append( female_names )
            all_female_counts.append( female_counts )
            all_male_names.append( male_names )
            all_male_counts.append( male_counts )

            #  Reset the lists for the next year
            female_names = []
            female_counts = []
            male_names = []
            male_counts = []
            year = int(line[0])

        #  Check for a well-formatted line for a female name
        elif len(line)==3 and line[1].lower()  == 'f' and line[2].isdigit():
            female_names.append( line[0] )
            female_counts.append( int(line[2]) )

        #  Check for a well-formatted line for a male name
        elif len(line)==3 and line[1].lower()  == 'm' and line[2].isdigit():
            male_names.append( line[0] )
            male_counts.append( int(line[2]) )

        #  If we get here there is a formatting error somewhere and we
        #  don't know what else to do so we quit
        else:
            print("Error: internal format on line number", line_num)
            return False

    #  We get to here after the entire file has been read.  We now
    #  need to save the last year's lists to the global lists.
    all_female_names.append( female_names )
    all_female_counts.append( female_counts )
    all_male_names.append( male_names )
    all_male_counts.append( male_counts )

    #  We are done!
    return True
            

def top_in_year( year, f_or_m ):
    ''' For the given year, access the list of names and the list of
        counts for the names.  Return empty lists if the year is out
        of range.
        '''
    if year < first_year or year > 2014:
        return ([], [])

    index = year - first_year
    if f_or_m.lower() == 'f':
        return (all_female_names[index], all_female_counts[index])
    else:
        return (all_male_names[index], all_male_counts[index])


'''
The following code is only run if the module is being executed as a program.
'''
if __name__ == "__main__":
    fn = "top_names_1880_to_2014.txt"

    if read_from_file( fn ):
        print("Successful read")
    else:
        print("Read failed")
        sys.exit()

    (names,counts) = top_in_year( 1883, 'F')
    for i in range(10):
        print(i, names[i], counts[i])

    print()
    (names,counts) = top_in_year( 1885, 'M')
    for i in range(10):
        print(i, names[i], counts[i])

        
    
