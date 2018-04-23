#!/usr/bin/env python3

import subprocess
import sys

#function to make sure we don't get an array out of bounds error when getting args
def checkNumArgs(numArgs):
	if len(sys.argv) < numArgs:
		print("ERROR!:", numArgs, "arguments required")
		print("only provided", len(sys.argv))
		sys.exit()	

#function to format json option and print it
def printOption(option):
	option = option[1:]
	option = option.lower()
	subprocess.call([commonASTtraversalPath, "out.txt", option])

#function to print the commonAST for either language in out.txt
def getCommonASTinOut(lang, filename):
	if lang == "-py":
		try:
			subprocess.check_output(["python3", astMatcher_pythonPath, filename])
		except subprocess.CalledProcessError as e:
			print (e.output)
			sys.exit()
	elif lang == "-cpp":
		f = open("out.txt", "w")
		try:
			subprocess.call([astMatcher_cppPath, filename], stdout=f)
		except subprocess.CalledProcessError as e:
			print (e.output)
			sys.exit()

	else:
		print ("invalid language")
		sys.exit()

#function to add all filenames in *.py to filenames
def expandStar(filename, filnames):
	for fname in glob.glob(filename):
		filenames.append(fname)

astMatcher_pythonPath = "/usr/local/submitty/SubmittyAnalysisTools/astMatcher.py"
astMatcher_cppPath = "/usr/local/submitty/clang-llvm/build/bin/ASTMatcher"
commonASTtraversalPath = "/usr/local/submitty/SubmittyAnalysisTools/commonASTCount.out"


#if there are less than 2 arguments, exit
checkNumArgs(2)

#if we're in count mode, lang is the first argument
#if we're printing json, that will be the first argument
langOrOption = sys.argv[1]

filenames = []
lang = None
countArg = None
countType = None
minNumArgs = None
filename = None

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

#add the first filename to filenames
if "*." in filename:
	#if we need to expand, do so
	expandStar(filename, filenames)
else:
	#otherwise, just append
	filenames.append(filename)


#loop to add all filenames to filenames
if len(sys.argv) > minNumArgs:
	count = minNumArgs
	#while there are still filenames to add
	while(count < len(sys.argv)):
		#if we need to expand, do so. Otherwise just add it
		if "*." in sys.argv[count]:
			expandStar(sys.argv[count], filenames)
		else:
			filenames.append(sys.argv[count])
		count+=1

total = 0
#Main loop
if filename[len(filename)-1] == " ":
	filename = filename[:len(filename)-1]

#for fname in filenames:
#first get the commonAST XML IR printed to out.txt
getCommonASTinOut(lang, filename)
#then either print or count nodes 
if countMode:
	total += int(subprocess.check_output([commonASTtraversalPath, "out.txt", countType, countArg]))
else:
	printOption(langOrOption)

#now print the total
if(countMode):
	print(total)
