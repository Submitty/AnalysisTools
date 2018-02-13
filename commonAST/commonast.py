#!/usr/bin/env python3

import subprocess
import sys

#function to make sure we don't get an array out of bounds error when getting args
def checkNumArgs(numArgs):
	if len(sys.argv) < numArgs:
		print("ERROR!:", numArgs, "arguments required")
		print("only provided", len(sys.argv))
		sys.exit()	

#To do:
#	more than one file?

#function to format json option and print it
def printOption(option):
	option = option[1:]
	option = option.lower()
	subprocess.call([commonASTtraversalPath, "out.txt", option])

def getCommonASTinOut(lang, filename):
	if lang == "-py":
		subprocess.call(["python3", astMatcher_pythonPath, filename])
	elif lang == "-cpp":
		f = open("out.txt", "w")
		subprocess.call([astMatcher_cppPath, filename], stdout=f)
	else:
		print ("invalid language")
		sys.exit()

#To do:
#	more than one file?

astMatcher_pythonPath = "/usr/local/submitty/SubmittyAnalysisTools/astMatcher.py"
astMatcher_cppPath = "/usr/local/submitty/clang-llvm/build/bin/ASTMatcher"
commonASTtraversalPath = "/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out"


#if there are less than 2 arguments, exit
checkNumArgs(2)

#if we're in count mode, lang is the first argument
#if we're printing json, that will be the first argument
langOrOption = sys.argv[1]

filenames = []
filename = None
lang = None
countArg = None
countType = None
minNumArgs = None

#find out if we're counting nodes or just printing json
countMode = False
if(langOrOption == "-py" or langOrOption == "-cpp"):
	countMode = True	
	minNumArgs = 5
	checkNumArgs(minNumArgs)
	#we're in count mode so the arguments are in this order
	lang = langOrOption
	countType = sys.argv[2]
	countArg = sys.argv[3]
	filename = sys.argv[4]
else:
	#we're in print mode so the arguments are in this order 
	minNumArgs = 4	
	checkNumArgs(minNumArgs)
	filename = sys.argv[2]
	lang = sys.argv[3]

filenames.append(filename)

if len(sys.argv) > minNumArgs:
	count = minNumArgs
	while(count < len(sys.argv)):
		filenames.append(sys.argv[count])
		count+=1

total = 0
for fname in filenames:
	getCommonASTinOut(lang, fname)
	if countMode:
		total += int(subprocess.check_output([commonASTtraversalPath, "out.txt", countType, countArg]))
	else:
		printOption(langOrOption)
if(countMode):
	print(total)
