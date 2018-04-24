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
	subprocess.call([unionTraversalPath, "out.txt", option])

#function to print the commonAST for either language in out.txt
def getCommonASTinOut(lang, filenames):
	f = open("out.txt", "w")
	try:
		subprocess.call([unionToolPath, filenames], stdout=f)
	except subprocess.CalledProcessError as e:
		print (e.output)
		sys.exit()

unionToolPath = "/usr/local/submitty/clang-llvm/build/bin/UnionTool"
unionTraversalPath = "/usr/local/submitty/SubmittyAnalysisTools/unionCount.out"


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
if(langOrOption == "-json" or langOrOption == "-JSON"):
	#we're in print mode so the arguments are in this order 
	minNumArgs = 3
	checkNumArgs(minNumArgs)
	filename = sys.argv[2]
else:
	countMode = True	
	minNumArgs = 5
	checkNumArgs(minNumArgs)
	#we're in count mode so the arguments are in this order
	countType = sys.argv[2]
	countArg = sys.argv[3]
	filename = sys.argv[4]



#loop to add all filenames to filenames
'''
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
'''


if filename[len(filename)-1] == " ":
	filename = filename[:len(filename)-1]

total = 0
#Main loop
getCommonASTinOut(lang, filename)
#then either print or count nodes 
if countMode:
	total += int(subprocess.check_output([unionTraversalPath, "out.txt", countType, countArg]))
else:
	printOption(langOrOption)

#now print the total
if(countMode):
	print(total)
