line = ""
n = 0
while n <= 8:
    line = line + ' ' + str(n)
    n += 1    

k = ""
i = 0
while i <= 8:
    r = 0
    while r <= 8:
        k = k + str(i) + ',' + str(r) + ' '
        r += 1
        if r == 3 or r == 6:
            k = k + ' '
        if r == 9:
            k = k + '\n'
    i += 1
    if i == 3 or i == 6:
        k = k + '\n'

row = ""
n = 0
s = 2
while n <= 8:
    row = row + str(s) + ',' + str(n) + ' '
    n += 1    

column = ""
t = 2
u = 0
while u <= 8:
    column = column + str(u) + ',' + str(t) + ' '
    u += 1

three = ""
i = 0
while i < 3:
    j = 0
    while j < 3:
        three = three + str(i) + ',' + str(j) + ' '
        j += 1
    three = three + '\n'
    i += 1
  