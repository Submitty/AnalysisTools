def closest2(L):
    L1 = L.copy()
    L1.sort()
    x = 0
    values = []
    for x in range(len(L1) - 1):
        diff = abs(L[x] - L[x+1])
        values.append((diff, L[x], L[x+1]))
                      
    values.sort()
    return (values[0][1], values[0][2])

L = [2, 5, 6, 8]
print(closest2(L))

L1 = [2, 4, 8, 9, 11, 12, 27, 36]
print(closest2(L1))