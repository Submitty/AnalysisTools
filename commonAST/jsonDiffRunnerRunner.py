import subprocess
import sys
import glob

#directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/py-test-files/out/*.txt"
#directory= "/mnt/c/Users/Elizabeth/Documents/submitty/AnalysisTools/commonAST/cpp-test-files/out/*.txt"
directory = sys.argv[1] + "*.txt"
lang = sys.argv[2]

open("report.txt", "w")

files = []

def getMatchB(matcha):
	index = matcha.find("Union")
	prefix = matcha[:index]
	print("prefix: ", prefix)
	for fname in glob.glob(directory):
		if prefix in fname and "Intersect" in fname:
			return fname 

for fname in glob.glob(directory):
	if "Union" in fname:
		matcha = fname
		matchb = getMatchB(matcha)
		print("matcha: ", matcha, "matchb", matchb)
		files.append((matcha, matchb))

for tup in files:
	subprocess.check_call(["python3", "jsonDiffRunner.py", tup[0], tup[1], "report.txt", lang])
