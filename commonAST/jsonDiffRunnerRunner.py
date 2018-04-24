import subprocess
import sys
import glob
import os

#directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/py-test-files/out/*.txt"
#directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/cpp-test-files/out/*.txt"
dirprefix = sys.argv[1]
directory = sys.argv[1] + "*.txt"
lang = sys.argv[2]

reportdir = os.path.join(dirprefix, "report.txt")
print("creating html files")

rf = open(reportdir, "r+")

files = []

def getMatchB(matcha):
	index = matcha.find("Union")
	prefix = matcha[:index]
	for root,dirs,fnames in os.walk(dirprefix,topdown=False): 
		for fname in fnames:
			if (prefix + "Intersect") in fname:
				return fname 

for root,dirs,fnames in os.walk(dirprefix,topdown=False): 
	for fname in fnames:
		if "Union" in fname:
			matcha = fname
			matchb = getMatchB(matcha)
			#print("matcha: ", matcha, "matchb", matchb)
			if not matcha == None and not matchb == None:
				files.append((os.path.join(root, matcha), os.path.join(root, matchb)))

for tup in files:
	subprocess.check_call(["python3", "/usr/local/submitty/SubmittyAnalysisTools/jsonDiffRunner.py", tup[0], tup[1], dirprefix, reportdir, lang])

#get final stats
content = rf.read()
avgs = 0
index = content.find("percent unmatched: ")
while index != -1:
	avg = content[index+len("percent unmatched: "):	content.find("\n", index)]
	#print("average: ", avg)
	avgs += float(avg)
	content = content[content.find("\n", index):]
	index = content.find("percent unmatched: ")


totalAverage = avgs / len(files)
rf.write("\nTotal Number of files: " + str(len(files)) + "\n")
rf.write("Average percent of unmatched nodes: " + str(totalAverage)) 
