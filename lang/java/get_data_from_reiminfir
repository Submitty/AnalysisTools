import sys

path = sys.argv[1]
output = "csv"
if (len(sys.argv) == 3):
	output = sys.argv[2]

File = path + "/new-result.csv"
with open(File, "rb") as f:
    for last in f: pass      # Loop through the whole file reading it all.

tokens = last.split()
labels = ['ReadOnly','PolyRead','Mutable','Total References','Total Methods','Pure Methods']
data = {}
count = 0
for token in tokens:
	if token[0].isdigit():
		data[labels[count]] = token
		count +=1

data['Pure Methods'] = data['Pure Methods'].split('(')[0]
outline = ""
if (output == "csv"):
	outline = data['Total References']+","+data["ReadOnly"]+","+data["PolyRead"]+","+data["Total Methods"]+","+data["Pure Methods"]+"\n"
elif (output == "percent_pure"):
	outline = float(data["Pure Methods"]) / float(data["Total Methods"]) * 100
#Log = open("output.csv", "a")
#Log.write(outline)
#Log.close()
print outline

# line structure
# WORDS readonlyref (num%) readonly, polyref (num%) and mutableref (num%) WORDS totalref WORDS totalmethod WORDS puremethods(num%)
 

