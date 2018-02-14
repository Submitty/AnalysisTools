fname = input('Please enter the file number: ') + '.txt'
parno = int(input('Please enter the paragraph number: '))
lineno = int(input('Please enter the line number: '))

def get_line(fname, parno, lineno):
    f = open(fname)
    n = 1
    c = 0
    temp = ''
    for line in f:
        while line == '\n':
            temp = line
            line = f.readline()

        if line != '\n' and temp == '\n':
            n += 1
        
        if n == parno:
            c += 1
            if c == lineno:
                return line
        temp = line
        
print(get_line(fname, parno, lineno))        