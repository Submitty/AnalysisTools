def get_words(description):
    words = set()
    description = description.replace('.',' ').replace(',', ' ').replace('()', ' ').replace('"', ' ')
    description = description.lower()
    description = description.split(' ')
    for i in range(len(description)):
        if len(description[i]) >= 4 and description[i].isalpha():
            words.add(description[i])
    return words
    
file = input('Enter a file dude: ')
file = file + '.txt'
file = open(file, 'r')
desc = file.read()
desc = desc.split('|')
print(get_words(desc[1]))
print(len(get_words(desc[1])))