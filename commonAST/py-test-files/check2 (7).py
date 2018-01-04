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
file1 = input('Enter a file dude: ')
file1 = file1 + '.txt'
file1 = open(file1, 'r')
desc1 = file1.read()
desc1 = desc1.split('|')
two_words = get_words(desc1[1])

in_one_words_not_in_two_words = one_words - two_words
in_both = one_words & two_words
in_two_words_not_in_one_words = two_words - one_words

print('Comparing clubs', file, 'and', file1,':')
print('Same words:', in_both)
print()
print('Unique to', desc[0] ,':', in_one_words_not_in_two_words)
print()
print('Unique to', desc1[0] ,':', in_two_words_not_in_one_words)