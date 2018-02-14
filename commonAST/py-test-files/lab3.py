import math
def bunnies(bpop, fpop):
    bpop1 = int(bpop)
    fpop1 = int(fpop)
    bpop_next = (10*bpop1)/(1+0.1*bpop1) - .05*bpop1*fpop1
    return bpop_next

def foxes(fpop, bpop):
    fpop1 = int(fpop)
    bpop1 = int(bpop)
    fpop_next = (.4 * fpop1 + .02 * fpop1 * bpop1)
    return fpop_next

bpop = input('Number of bunnies ==> ')
print(bpop)
fpop = input('Number of foxes ==> ')
print(fpop)
print('Year 1:', bpop, fpop)

bpop_next = int(bunnies(bpop, fpop))
fpop_next = int(foxes(fpop, bpop))
print('Year 2:', bpop_next, fpop_next)

bpop = int(bunnies(bpop_next, fpop_next))
fpop = int(foxes(fpop_next, bpop_next))
print('Year 3:', bpop, fpop)

bpop_next = int(bunnies(bpop, fpop))
fpop_next = int(foxes(fpop, bpop))
print('Year 4:', bpop_next, fpop_next)

bpop = int(bunnies(bpop_next, fpop_next))
fpop = int(foxes(fpop, bpop))
print('Year 5:', bpop, fpop)