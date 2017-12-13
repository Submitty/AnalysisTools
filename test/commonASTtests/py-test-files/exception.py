def div(a,b):
	if b == 0:
		raise ZeroDivisionError('division by zero')
	return a/b

try: 
	print div(10,5)
	div(2,0)
	x = 1
except:
	print "exception"
	y = 1
