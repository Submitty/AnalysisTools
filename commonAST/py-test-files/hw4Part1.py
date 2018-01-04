#FUNCTION IS INTRODUCED
def is_alternating(word):
    word = word.lower() #MAKE LOWER CASE SO ALL LETTERS ARE WELCOME
    if word[0] in 'aeiou' or len(word) < 8: #STARTS WITH VOWEL OR LESS THAN 8, QUIT
        return False
    for n in range(len(word)):
        if n % 2 == 0 and word[n] in 'aeiou':   #EVEN NUMBERED LETTERS ARE VOWELS
            return False
        elif n % 2 == 1 and word[n] not in 'aeiou': #ODD NUMBERED LETTERS ARE CONSONANTS
            return False
        if n % 2 == 0 and n != 0:
            if word[n] >= word[n - 2]:  #LETTERS DECREASING
                return False
    return True #EVERYTHING ELSE IS FALSE

#INPUT                                          
word = input('Enter a word: ')
print(word)
print()

while (word.strip() != ""):
    if is_alternating(word):    #MEANS ALL IS TRUE
        print('The word', "'"+word+"'", 'is alternating')
        print()
    else:   #ONE OR MORE FALSE
        print('The word', "'"+word+"'", 'is not alternating')
        print()
    word = input('Enter a word: ')  #CONTINUE ASKING
    print(word)
    print()