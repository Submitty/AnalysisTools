import subprocess
import sys
import glob

#run sumbitty count

def testLang(lang):
	directory = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
	if(lang == "python"):
		directory += "*.py"	
	elif(lang == "c"):
		directory += "*.cpp"

	for fname in glob.glob(directory):
		subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/count", "-l", lang, whatToCount, fname])
 

 #node -l python for /var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/*.py

testLang("python", "for")
testLang("c", "for")
testLang("python", "while")
testLang("c", "while")
