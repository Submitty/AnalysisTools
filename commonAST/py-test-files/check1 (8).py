def add(m, n):
    if n == 0:
        return m
    else:
        return add(m, n-1) + 1
    
def mult(m, n):
    if m == 0:
        return 0
    elif m == 1:
        return n
    else:
        return add(mult(m-1, n), n) 
    
print(add(5,3))    
print(mult(5, 3))
print(mult(8, 3))

def power(x, n):
    if x == 0:
        return 0
    elif n == 0:
        return 1
    elif n == 1:
        return x
    elif x == 1:
        return 1
    else:
        return mult(x, power(x, n -1))
    
print(power(5, 4))