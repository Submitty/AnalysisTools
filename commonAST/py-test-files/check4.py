first = input('Please enter your first name: ')
last = input('Please enter your last name: ')
hi = 'Hello, '
length0 = len(hi)
length1 = len(first)
length2 = len(last ) + 1
length = max(length1, length2, length0)
print(length==length2)
print('\n','*' * (length + 6), sep = '')
print(('*' * 2), 'Hello,',' ' * (length-length0), '*' * 2)
print(('*' * 2), first+ ' ' * (length-length1), '*' * 2)
print(('*' * 2), last+ '!'+ ' ' * (length-length2), '*' * 2)
print('*' * (length + 6))