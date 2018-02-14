import lab05_util
restaurants = lab05_util.read_yelp('yelp.txt')

def print_info(n):
    (name, latitude, longitude, address, URL, style, scores) = restaurants[n]
    print(name, '(' + style + ')') 
    address = address.split('+')
    print('\t', address[0], '\n\t', address[1])
    avg = sum(scores)/len(scores)
    print('Average Score: {:.2f}'.format(avg))
    
print_info(0)    
print_info(1)    