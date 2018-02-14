'''
Initial example to demonstrate how to read and access the baby names
and counts.
'''
import read_names

# Read in all the names.  The result is stored in the module.
read_names.read_from_file("top_names_1880_to_2014.txt")

# Access the female list and counts for 1886
(female_names,female_counts) = read_names.top_in_year(1886, 'f') 
print("The most common female name in 1886, with a count of {:d}, is {:s}"\
    .format(female_counts[0], female_names[0]))

# Access the male list and counts for 1997.  Note that the 100th most
# popular name is in position 99 of the list.
(male_names,male_counts) = read_names.top_in_year(1997, 'M') 
print("The 100th most common male name in 1997, with a count of {:d}, is {:s}"\
    .format(male_counts[99], male_names[99]))
    
