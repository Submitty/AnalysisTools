import subprocess
import sys
import glob

#directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/py-test-files/out/*.txt"
directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/cpp-test-files/out/*.txt"

open("report.txt", "w")

count = 1
prev = ""
files = []

for fname in glob.glob(directory):
	if count % 2 == 0:
		files.append((fname, prev))
	prev = fname	
	count += 1

for tup in files:
	subprocess.check_call(["python3", "jsonDiffRunner.py", tup[0], tup[1], "report.txt"])
