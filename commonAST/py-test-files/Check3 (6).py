def parse_line(words):
    l = []
    l = words.split('/')
    new_line = []
    string = ''
    tup = ()
    
    n = -1
    while n > -4:
            if l[n].strip().isdigit():
                    num = (int(l[n]),)
                    tup = tup + num
            else:
                    return None
            n -= 1
    tup = tup[::-1]
    
    for j in range(len(l)-3):
        new_line.append(l[j])
    
    string = '/'.join(new_line)
    s = (string,)
    
    tup = tup + s
    return tup

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
        
f_out = open('program.py', 'w')
j = get_line(fname, parno, lineno)      
j = parse_line(j)

while j[3] != 'END':
    f_out.write(j[3]+"\n")
    j = get_line(str(j[0])+'.txt',j[1],j[2])
    j = parse_line(j)

f_out.close()    