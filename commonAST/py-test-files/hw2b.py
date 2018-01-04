# function for encrypting
def encrypt(word):
    final = word.replace(' a','%4%').replace('he','7!').replace('e', '9(*9(').replace('y','*%$').replace('u','@@@').replace('an','-?').replace('th', '!@+3').replace('o','7654').replace('9','2').replace('ck','%4')    #all replaces in one variable
    return final

#function for decrypting
def decrypt(word):
    done = word.replace('%4','ck').replace('2','9').replace('7654','o').replace('!@+3', 'th').replace('-?','an').replace('@@@','u').replace('*%$','y').replace('9(*9(','e').replace('7!','he').replace('%4%',' a')  #reversed to do opposite job
    return done

#variables and printing
word = input('Enter a string to encode ==> ')
print(word)
word2 = encrypt(word)
print() #print blank line
#print first function
print('Encrypted as ==>', encrypt(word))
length_word = len(word)
length_encripted = len(encrypt(word))
difference_length = abs(length_word - length_encripted) #absolute value
print('Difference in length ==>', difference_length)
#print second function
print('Deciphered as ==>', decrypt(word2)) 
#if and elif
if decrypt(word2) != word:
    print('Operation is not reversible on the string.')
    
elif decrypt(word2) == word:    #decrypt the encrypted word, not original
    print('Operation is reversible on the string.')