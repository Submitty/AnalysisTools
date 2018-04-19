import subprocess
import sys
import glob


lang = sys.argv[2]
if not lang == "py" and not lang == "cpp":
	print("Language not supported. Use 'py' or 'cpp'")


#directoryPrefix = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
directoryPrefix = sys.argv[1]
directory = directoryPrefix + "*." + lang


for fname in glob.glob(directory):
	filename_full = fname 
	print("=================")
	print(filename_full)
	print("=================")
	end = filename_full.rfind(".")
	
	filename_extStripped = filename_full[:end]	
	start = filename_extStripped.rfind("/") + 1
	filename_extPrefixStripped = filename_extStripped[start:]

	if lang == "py":
		out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/diagnostics", "-l", "python", filename_full])
	elif lang == "cpp":
		out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/unionToolRunner.py", "-json", fname])

	fw = open(directoryPrefix + "out/" + filename_extPrefixStripped + "Union.txt", "w")
	fw.write(out.decode('utf-8'))


	out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-json", fname, "-" + lang ])
	fw2 = open(directoryPrefix + "out/" + filename_extPrefixStripped + "Intersect.txt", "w")
	fw2.write(out.decode('utf-8'))

