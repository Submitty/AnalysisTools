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
#filename = sys.argv[2]
countType = sys.argv[2]
countArg = sys.argv[3]

filenames = [sys.argv[4]]

#data[filename] = (countType, countArg)

if len(sys.argv) > 5:
	count = 5
	while(count < len(sys.argv)):
		#print("adding: ", sys.argv[count], " to data with args: ", sys.argv[count+1], "and", sys.argv[count+2])
		filenames.append(sys.argv[count])
		#data[sys.argv[count]] = (sys.argv[count+1], sys.argv[count+2])
		#count+=3
		count+=1

if lang == "-py":
	for fname in filenames:
		#subprocess.call(["python", "/usr/local/submitty/SubmittyAnalysisTools/astMatcher.py", fname])
		subprocess.call(["python3", "astMatcher.py", fname])
		subprocess.call(["../../../spring2017/commonASTs/count.out", "out.txt", countType, countArg])
	
elif lang == "-cpp":
	for fname in filenames:
		f = open("out.txt", "w")
		alias = "~/clang-llvm/build/bin/ASTMatcher" #+ filename
		subprocess.call([alias, fname], stdout=f)
		#p = subprocess.Popen(["/bin/bash", "-i", "-c", alias], stdout=f)
		#subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out", "out.txt", countType, countArg])
		subprocess.call(["../../../spring2017/commonASTs/count.out", "out.txt", countType, countArg])
else:
	print ("invalid language")
	#sys.exit();

