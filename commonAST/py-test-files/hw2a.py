#functions for reversing
def reverse3(given):
    digit1 = given//100
    digit2 = ((given % 100)//10)
    digit3 = given % 10
    return (digit3*100+digit2*10+digit1)

def reverse5(given):
    digit1 = given//10000
    digit2 = ((given % 10000)//1000)
    digit3 = ((given % 1000)//100)
    digit4 = ((given % 100)//10)
    digit5 = given % 10
    return (digit5*10000+digit4*1000+digit3*100+digit2*10+digit1)
#prints       
print('Enter a 5 digit number whose second and fourth digits must differ by at least 2.')
print('The answer will be 1089, if your number is valid')
value = input('Enter a value ==> ')
print(value)
number = int(value) #make it an integer
print() #print a blank line
print('Here is the computation:')
print(number, 'reversed is', reverse5(number))  #reverse function
first_three = ((number%10000)//10) #first three digit number seen
second_three = reverse3(first_three)    #reverse function
subtraction_result = abs(second_three - first_three)    #absolute value
print(second_three, '-', first_three, '=', subtraction_result)
print(subtraction_result, '+', reverse3(subtraction_result), '=', subtraction_result+reverse3(subtraction_result))  #reverse function
print('You see, I told you.')