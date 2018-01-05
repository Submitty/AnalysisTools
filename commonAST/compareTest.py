import subprocess
import sys
import glob

#run sumbitty count

def testLang(lang, whatToCount, arg):
	directory = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
	if(lang == "python"):
		directory += "*.py"	
		commonastlang = "py"
	elif(lang == "c"):
		directory += "*.cpp"
		commonastlang = "cpp"

	for fname in glob.glob(directory):
		#print(fname)
		submittyCount = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/count", "node", "-l", lang, whatToCount, fname])

		#call = "/usr/local/submitty/SubmittyAnalysisTools/count node -l " + lang + " " + whatToCount + " " + fname
		#submittyCount = subprocess.Popen(["/bin/bash", "-i", "-c", call])
		commonAST = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-"+commonastlang, "-"+whatToCount.capitalize(), fname, arg])
		#call = "/usr/local/submitty/SubmittyAnalysisTools/commonast.py " + "-"+commonastlang + " -"+whatToCount.capitalize() + " " + fname + " " + arg
		#commonAST = subprocess.Popen(["/bin/bash", "-i", "-c", call])

		print("EQUAL?", submittyCount == commonAST)

 #node -l python for /var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/*.py

print("number of for loops")
testLang("python", "for", "Void")
testLang("c", "for", "Void")
print("number of while loops")
testLang("python", "while", "Void")
testLang("c", "while", "Void")
