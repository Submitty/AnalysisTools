import sys


def cutEnding(word):
	count = 0
	index = len(word)-1
	lastBracket = 0

	while not (word[index] == "]"):
		if word[index] == "}":
			count += 1		
			lastBracket = index
		index -= 1
	
	if count > 1:
		return word[:lastBracket+1]

	return word

if len(sys.argv) < 2:
	print("error: must specify a file")
	exit()

fw =  open(sys.argv[1], "r") 
content = fw.read()
index = content.find("nodes")

if index > -1:
	content = content[index+7:]

content = cutEnding(content)

fw2 =  open(sys.argv[1], "w") 
fw2.write(content)
