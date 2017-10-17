a = False

k = 0
if k == 0:
	k = k+1;
	a = True
	b = not a and a or a and a
elif k > 0:
	k = k+2;
elif k < 0:
	k = k+3;
elif not b:
	k = -1;
else:
	b = True
