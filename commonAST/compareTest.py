import subprocess
import sys
import glob

#run sumbitty count

def testLang(lang, whatToCount, arg):
	global countEqual
	global countUnEqual
	directory = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
	if(lang == "python"):
		directory += "*.py"	
		commonastlang = "py"
	elif(lang == "c"):
		directory += "*.cpp"
		commonastlang = "cpp"

	for fname in glob.glob(directory):
		#print(fname)
		if(whatToCount == "call"):
			submittyCount = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/count", "call", whatToCount, "-l", lang, fname])
		else:

			submittyCount = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/count", "node", "-l", lang, whatToCount, fname])

		commonAST = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-"+commonastlang, "-"+whatToCount.capitalize(), arg, fname])

		#print(submittyCount, commonAST)
		equal = submittyCount == commonAST
		print("EQUAL?", equal)
		if equal:
			countEqual += 1
		else:
			countUnEqual += 1

 #node -l python for /var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/*.py

countEqual = 0
countUnEqual = 0

'''
print("number of for loops")
testLang("python", "for", "Void")
testLang("c", "for", "Void")

print("number of while loops")
testLang("python", "while", "Void")
testLang("c", "while", "Void")
'''

print("number of student defined func: check1")
testLang("python", "call", "check1")
testLang("c", "call", "check1")

print("\n", countEqual, "/", countUnEqual+countEqual)
