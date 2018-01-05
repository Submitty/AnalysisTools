import subprocess
import sys
import glob

#run sumbitty count

def testLang(lang, whatToCount):
	directory = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
	if(lang == "python"):
		directory += "*.py"	
		commonastlang = "py"
	elif(lang == "c"):
		directory += "*.cpp"
		commonastlang = "cpp"

	for fname in glob.glob(directory):
		#print(fname)
		submittyCount = subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/count", "node", "-l", lang, whatToCount, fname])
		commonAST = subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-"+commonastlang, "-"+whatToCount.capitalize(), fname])

		print("EQUAL?", submittyCount == commonAST)

 #node -l python for /var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/*.py

print("number of for loops")
testLang("python", "for")
testLang("c", "for")
print("number of while loops")
testLang("python", "while")
testLang("c", "while")
