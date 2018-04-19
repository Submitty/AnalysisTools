import subprocess
import sys
import glob


directoryPrefix = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
directory = directoryPrefix + "*.py"


for fname in glob.glob(directory):
	filename_full = fname 
	print("=================")
	print(filename_full)
	print("=================")
	end = filename_full.rfind(".")
	
	filename_extStripped = filename_full[:end]	
	start = filename_extStripped.rfind("/") + 1
	filename_extPrefixStripped = filename_extStripped[start:]

	#out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/diagnostics", "-l", "python", filename_full])

	out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-json", fname, "-py" ])

	fw = open(directoryPrefix + "out/" + filename_extPrefixStripped + "Intersect.txt", "w")
	fw.write(out.decode('utf-8'))

