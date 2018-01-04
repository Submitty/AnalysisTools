def parse_line(words):
    l = []
    l = words.split('/')
    new_line = []
    string = ''
    tup = ()
    
    n = -1
    while n > -4:
        if l[n].isdigit():
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
    print(tup)
    
parse_line('Here is some random text, like 5/4=3./12/3/4')    