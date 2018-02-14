"""
This is a helper module to be used for Lab 05. The function
read_yelp takes as input the name of the yelp data file, and
returns a list containing restaurants. Use it as::

    import lab05_util
    restaurants = lab05_util.read_yelp(filename)

Also, you can test its contents as:

    print(restaurants[0])
"""

def parse_line(line):
    """
    Parses a single line of the yelp file, keeping some of the
    data, and throwing away the rest.
    """
    line = line.strip('\n')
    values = line.split('|')
    s_rating = values[6:]
    scores = []
    for s in s_rating:
        scores.append( int(s) )
    result = [ values[0], \
               float(values[1]),\
               float(values[2]), \
               values[3], \
               values[4], \
               values[5], \
               scores ]
    return result

def read_yelp(filename):
    """
    Parses the given filename containing yelp data and
    returns a list of restaurants. Each item is a list containing 
    restaurant information.
    """
    restaurants = []
    for line in open(filename):
        new_r = parse_line(line)
        restaurants.append(new_r)
    return restaurants
