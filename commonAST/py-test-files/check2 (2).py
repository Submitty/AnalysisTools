import lab05_util
import sys
restaurants = lab05_util.read_yelp('yelp.txt')

def print_info(n):
    (name, latitude, longitude, address, URL, style, scores) = restaurants[n]
    print(name, '(' + style + ')') 
    address = address.split('+')
    print('\t', address[0], '\n\t', address[1])
    if len(scores) < 3:
        avg = (sum(scores) - max(scores) - min(scores))/(len(scores)-2)
    else:
        average = sum(scores)/len(scores)
    if average < 2:
        print('This restaurant is rated bad, based on', len(scores), 'reviews.')
    if 2 < average < 3:
        print('This restaurant is rated average, based on', len(scores), 'reviews.')
    if 3 < average < 4:
        print('This restaurant is rated above average, based on', len(scores), 'reviews.')
    if 4 < average < 5:
        print('This restaurant is rated very good, based on', len(scores), 'reviews.')      
    
id = int(input('Enter a restaurant id: ')) - 1
if id < 1 or id > len(restaurants):
    print('This is not a valid restaurant id\n')
    sys.exit()
    
print_info(id)
    