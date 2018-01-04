from Check2 import *
filename = 'birthdays.txt'
f = open(filename)

dates = []
for line in f:
    temp = line.strip().split(' ')
    date = Date(int(temp[0]), int(temp[1]), int(temp[2]))
    dates.append(date)
    
dates.sort()
print(dates[0])
print(dates[-1])
'''  
i = 0
m = Date(9999, 99, 99)
temp = ''
while i < len(dates) - 1:
    if dates[i] < dates[i+1]:
        temp = dates[i]
    if temp < m or temp == m:
        m = temp
    i += 1
print(m)

j = 0
x = Date(0000, 00, 00)
temp = Date(0000, 00, 00)
while j < len(dates) - 1:
    if not dates[j] < dates[j+1]:
        temp = dates[j]
    if not temp < x or temp == x:
        x = temp
    j += 1
print(x)
'''

months = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
for date in dates:
    mon = date.month
    months[mon] += 1
month = months.index(max(months)) 
print(month_names[month])