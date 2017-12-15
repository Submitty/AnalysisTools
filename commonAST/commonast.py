#!/usr/bin/env python3

import subprocess
import sys

#To do:
#	more than one file?
#	fix python errors
#	fix why its running python
#	documentation?

if len(sys.argv) < 5:
	print("ERROR!: 5 arguments required")
	print("only provided", len(sys.argv))
	sys.exit()	

lang = sys.argv[1]
filename = sys.argv[2]
countType = sys.argv[3]
countArg = sys.argv[4]  	

data = {}
data[filename] = (countType, countArg)

if len(sys.argv) > 5:
	count = 5
	while(count+2 < len(sys.argv)):
		data[sys.argv[count]] = (sys.argv[count+1], sys.argv[count+2])
		count+=2

if lang == "-py":
	subprocess.call(["python", "/usr/local/submitty/SubmittyAnalysisTools/astMatcher.py", filename])
elif lang == "-cpp":
	f = open("out.txt", "w")
	alias = "/usr/local/submitty/clang-llvm/build/bin/ASTMatcher" #+ filename
	subprocess.call([alias, filename], stdout=f)
	#p = subprocess.Popen(["/bin/bash", "-i", "-c", alias], stdout=f)
else:
	print ("invalid language")
	#sys.exit();

for key in data:
	subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out", "out.txt", data[key][0], data[key][1]])
