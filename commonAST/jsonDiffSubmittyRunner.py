import subprocess
import sys
import glob
import json
import os

inputdir = sys.argv[1]
outputdir = sys.argv[2]
lang = sys.argv[3]

subdirs =  [name for name in os.listdir(inputdir)
            if os.path.isdir(os.path.join(inputdir, name))]	


sourceFiles = []

#subdirs are student folders
for subdir in subdirs:
	if not os.path.isdir(inputdir + subdir + "/user_assignment_settings.json"): continue

	jsonFile = open(inputdir + subdir + "/user_assignment_settings.json")

	assignmentSettings = json.load(jsonFile)
	nextFolder = str(assignmentSettings["active_version"])

	sourceFile = (subdir + "/" + nextFolder + "/*" + lang)
	
	sourceFiles.append(sourceFile)

for sourceFile in sourceFiles:
	#create the union file
	#create the commonAST file > put all of these "fileUnion.txt" files in the output folder
	subprocess.check_call(["python3", "jsonDiffRunnerRunner.py", "outputDirectory", lang])
