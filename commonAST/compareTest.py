import subprocess
import sys
import glob

#run sumbitty count

testLang(lang):
	directory = "/var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/"
	if(lang == "python"):
		directory += "*.py"	
	elif(lang == "c"):
		directory += "*.cpp"

	for fname in glob.glob(directory):
		subprocess.call(["/usr/local/submitty/SubmittyAnalysisTools/count", "-l", lang, whatToCount, fname)
 

 #node -l python for /var/local/submitty/courses/f17/tutorial/submissions/for_test_cpp/instructor/13/part1/*.py

testLang("python")
testLang("c")
