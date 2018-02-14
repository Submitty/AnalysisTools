'''
A short example program to illustrate the formation and use of list
containing sets and list containing lists containing sets.  Here, each
individual list is a state, and the components of the list are the
population rank of that state (Califonia is first), the name of that
state, and a set of companies in that state.

The actual problems involving find the companies in common between
states and, for a given state, finding what companies it has that no
other state does.
'''

def in_common( state1, state2 ):
    '''
    Find and output, in order, the companies that the two states have in common.

    Starting by making a list from the intersection of the two sets of
    companies.  We do this so we can sort it into alphabetical order.
    '''
    common = list(state1[2] & state2[2]) 
    common.sort()

    if len(common) == 0:
        print("States {} and {} have no companies in common".format(state1[1], state2[1]))
    else:
        print("States {} and {} have {} companies in common".format(state1[1], state2[1], len(common)))
        print("Here they are in alphabetical order:")
        i = 0
        for i in range(len(common)):
            print('\t{}: {}'.format(i,common[i]))

def only_in_state( state, all_states):
    '''
    For a particular state, find the companies that are only in that state.
    '''
    only_in_state = state[2]
    for other_state in all_states:
        if state[0] != other_state[0]:   # skip the target state so we don't remove everything
            only_in_state = only_in_state - other_state[2]
    only_in_state = list(only_in_state)  # convert the set to a list and then sort it
    only_in_state.sort()

    if len(only_in_state) == 0:
        print('No companies are only in state {}'.format(state[0]))
    else:
        #  Output on one line.
        out_string = ''
        for s_name in only_in_state:
            out_string += s_name + ' '
        out_string = out_string.strip()  # get rid of last blank
        print("The following companies are only in state {}: {}".format(state[0], out_string))


if __name__ == "__main__":
    '''
    Build the list of lists of states.  Each state is represented by a
    list of an integer, a string and a set of strings.
    '''
    all_states = []
    companies = set(['google', 'microsoft', 'amazon'])
    state = [ 13, 'washington', companies ]
    all_states.append( state )

    companies = set(['amd', 'ibm', 'dell'])
    state = [ 2, 'texas', companies ]
    all_states.append(state)

    companies = set(['ibm', 'google', 'bloomberg', 'goldman', 'amazon'])
    state = [ 3, 'new york', companies ]
    all_states.append( state )

    companies = set(['apple', 'facebook', 'intel', 'google'])
    state = [ 1, 'california', companies ]
    all_states.append( state )

    all_states.sort(reverse=True)   # sort the list of states in decreasing order of population
    print("The least populous state in the list, with rank {}, is {}".format(all_states[0][0], all_states[0][1]))

    print()
    print("Checking companies the least populous state has in common with other states")
    for i in range(1,len(all_states)):
        print()
        in_common(all_states[0], all_states[i])

    # Check for companies that are only in this state
    print()
    only_in_state( all_states[0], all_states )
    
