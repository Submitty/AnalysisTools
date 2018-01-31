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
countType = sys.argv[2]
countArg = sys.argv[3]

filenames = [sys.argv[4]]

if(len(sys.argv) > 5):
	option = sys.argv[5] 	
	option = option[1]
	option = option.lower()

if len(sys.argv) > 5:
	count = 5
	while(count < len(sys.argv)):
		filenames.append(sys.argv[count])
		count+=1

if lang == "-py":
	for fname in filenames:
		subprocess.call(["python", "/usr/local/submitty/SubmittyAnalysisTools/astMatcher.py", fname])
		#subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out", "out.txt", countType, countArg])
	
elif lang == "-cpp":
	for fname in filenames:
		f = open("out.txt", "w")
		alias = "/usr/local/submitty/clang-llvm/build/bin/ASTMatcher"
		subprocess.call([alias, fname], stdout=f)
		#subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out", "out.txt", countType, countArg])
else:
	print ("invalid language")
	sys.exit()

subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out", "out.txt", countType, countArg, option])


