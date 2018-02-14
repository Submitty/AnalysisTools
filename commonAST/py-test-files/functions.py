import math

def func(a, b, c):
	print (a, b, c)

def addOne(a):
	return a+1

def subOne(a):
	return a-1


p = 0
p = math.ceil(5.5)
for i in range(0, 5):
	k = 0
	j = addOne(math.ceil(i))
	#print i, j
	while j > 0:
		j = subOne(j)	
		if i == j:
			r = "test"
			p = 1
			#print "same!"
			if j < 1:
				p = 7
		else:
			k+=1
			if k > 10:
				p = 2
				#print "BIG"
