import lab06_util

def verify_board():
    i = 0
    while i <= 8:
        j = 0
        while j <= 8:
            if number == '.':
                return False
            else:
                x = ok_to_add(i, j, number)
                return True
    if True:
        print('The solution is valid')
                
            
                
def ok_to_add(row, column, number):
    row = row - 1
    column = column - 1
    numbers = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    value = 0
    while value <= 8:
        x = bd[row][value]
        if str(number) == x:
            print('This number cannot be added')
            return False
        value += 1
    value = 0
    while value <= 8:
        x = bd[value][column]
        if str(number) == x:
            print('This number cannot be added')
            return False
        value += 1
        
    column2 = column//3
    column2 = column * 3
    row2 = row//3
    row2 = row *3
    for i in range(3):
        for j in range(3):
            if bd[row2 + i][column2 + j] == number:
                print('This number cannot be added')
                return False      
        
        
    if bd[row][column] in numbers:
        print(bd[row][column])
        print('This number cannot be added')
        return False
    else:
        bd[row][column] = number
        print_board(bd)
    return True

def print_board(bd):
    row_spacer = '-------------------------'
    i = 0
    row = []

    rows = ['r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9', 'r10', 'r11']
    numbers = []
    j = 0
    i = 0
    for l in rows:
        rows[i] = []
        i += 1

    while j <= 8:
        k = 0
        while k <= 8:
            value = str(bd[j][k])
            if (k == 8):
                value = value + ' |'
            if (k ==0) or (k ==3) or (k== 6):
                value = '| ' + value
            rows[j].append(value)
            k += 1
            
        if (j == 0) or (j == 3) or (j == 6):
            print(row_spacer)
        print(' '.join(rows[j]))
        if (j == 8):
            print(row_spacer)
        j += 1    

board = input('Enter a board name thing: ')
bd = lab06_util.read_sudoku(board)

print_board(bd)   
row = int(input('Enter a row: ')) 
print(row)
column = int(input('Enter a column: ')) 
print(column)
number = int(input('Enter a number: ')) 
print(number)

ok_to_add(row, column, number)
