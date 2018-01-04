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
one_words = get_words(desc[1])
club1 = desc[0]

allclubs = open('allclubs.txt')

i = 0
amount_similar = []
for clubs in allclubs:
    x = clubs.split('|')
    if x[0] != club1:
        similar = get_words(x[1]) & one_words
        amount = len(similar)
        amount_similar.append((amount, x[0]))
                
amount_similar.sort()
print(amount_similar[-1][1])
print(amount_similar[-2][1])
print(amount_similar[-3][1])
print(amount_similar[-4][1])
print(amount_similar[-5][1])