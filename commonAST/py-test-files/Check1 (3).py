import random
import time

def closest1(L):
    '''
    >>> closest1([15.1, -12.1, 5.4, 11.8, 17.4, 4.3, 6.9])
    (5.4, 4.3)
    >>> closest1([2])
    (None, None)
    >>> closest1([2, 2, 5, 5, 7, 8])
    (2, 2)
    '''
    if len(L) < 2:  
        return (None, None)
    lit = []
    for x in range(len(L)):
        for i in range(x + 1, len(L)):
            value = L[i]
            difference = abs(L[x]-L[i])
            lit.append((difference, L[x], L[i]))
    lit.sort()            
    closest = (lit[0][2], lit[0][1])
    return closest

def closest2(L):
    '''
    >>> closest2([15.1, -12.1, 5.4, 11.8, 17.4, 4.3, 6.9])
    (4.3, 5.4)
    >>> closest2([2, 2, 5, 5, 7, 8])
    (2, 2)
    '''    
    L1 = sorted(L)
    values = []
    for x in range(len(L1) - 1):
        diff = abs(L1[x] - L1[x+1])
        values.append([diff, L1[x], L1[x+1]])
                      
    values.sort()
    return (values[0][1], values[0][2])

if __name__ == '__main__':
    N = 10000
    suz = []
    for i in range(N):
        suz.append(random.uniform(0.0, 1000.0))
    t1 = time.time()
    print(closest1(suz))
    print(time.time() - t1)
    t2 = time.time()
    print(closest2(suz))
    print(time.time() - t2)
    