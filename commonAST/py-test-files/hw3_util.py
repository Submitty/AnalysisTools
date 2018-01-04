"""
   Utility functions for Homework #3 Fall 2014

   To use this module, first import it

   import hw3_util

"""

def read_fifa():
    """ Reads the file containing team scores, and returns a list.
        Each item in the list is a list containing:
            [group id, team country, games_played, 
             win, draw, loose, goals for, goals against]
    """
    
    teams = []
    for line in open("team_scores.txt"):
        m = line.strip().split(",")
        for i in range(len(m)):
            if i != 1:
                m[i] = int(m[i])
        teams.append(m)
    return teams


def read_legos(filename):
    """This function is to be used for part2 of the homework.

    Read a file containing one lego type per line, and each line containing
    the type of lego and the number, separated by a comma. It returns a list
    for all the legos read from the file.
    
    Call this function as:

    mylegos = hw3_util.read_legos(filename)
    
    where mylegos is a list of all your legos.

    For example, if you are given the following file contents:

    1x1, 6
    2x1, 2
    2x2, 2
    2x4, 1

    The above call will return the following list:

    ['1x1', '1x1', '1x1', '1x1', '1x1', '1x1', 
     '2x1', '2x1', '2x2', '2x2', '2x4']
    
    """
    
    all_legos = []
    for line in open(filename):
        line = line.strip("\n")
        lego_info = line.split(",")
        lego_type = lego_info[0].strip()
        lego_count = int(lego_info[1])
        for i in range(lego_count):
            all_legos.append(lego_type)
    return all_legos
        


if __name__ == "__main__":
    legos = read_legos("legos.txt")
    print(legos)
