""" This is a utility module for Homework#4 in CSCI 1100 Fall 2015

    For part 1, use the read_pokemon function to read the pokemon
    appearance information as follows:

    import hw4_util
    time, pokemon, x, y = read_pokemon()


    For part 3, use the read_deaths function to read the deaths related data 
    for a county as follows:

    import hw4_util
    cdata = hw4_util.read_deaths('US') ##for county='US'


"""

def read_pokemon():
    turns = []
    pokemon = []
    pos = []
    y = []
    for line in open('pokemon.txt'):
        m = line.strip().split()
        turns.append(int(m[0]))
        pokemon.append(m[1])
        pos.append((int(m[2]), int(m[3])))
    return pokemon, pos     
    
def read_deaths(county):
    dates, counties = read_deaths_all()
    for county_data in counties:
        if county_data[0].lower() == county.lower():
            return county_data[1:]
    return []

def read_deaths_all():
    i = 0
    header = []
    counties = []
    dates = []
    for line in open('nys_deaths.csv').read().split("\n"):
        m = line.strip().split(",")
        i += 1
        if i == 1:
            for val in m[1:]:
                counties.append( [val] )
        else:
            dates.append(m[0])
            for i in range(1,len(m)):
                val = float(m[i])
                counties[i-1].append(val)
    return dates, counties

def read_legos():
    legos = []
    for line in open('legos.txt'):
        m = line.strip().split()
        cnt = int(m[0])
        lego = m[1]
        legos += [lego]*cnt
    return legos

if __name__ == "__main__":
    ## Example use
    data = read_deaths('Allegany')
    print(data)

    pokemon = read_pokemon()
    print(pokemon)
