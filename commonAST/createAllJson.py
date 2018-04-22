import subprocess
import sys
import glob
import os


lang = sys.argv[2]
if not lang == "py" and not lang == "cpp":
	print("Language", lang, "not supported. Use 'py' or 'cpp'")


#directoryPrefix = "/usr/local/submitty/GIT_CHECKOUT_AnalysisTools/commonAST/py-test-files/"
directoryPrefix = sys.argv[1]
directory = directoryPrefix + "*." + lang

for root, dirs, files in os.walk(directoryPrefix, topdown=False):
	for fname in files:
		if not fname.endswith("." + lang): continue
		filename_full = os.path.join(root, fname)
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
			out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/unionToolRunner.py", "-json", filename_full])
		
		fw = open(directoryPrefix + "/out/" + filename_extPrefixStripped + "Union.txt", "w")
		fw.write(out.decode('utf-8'))
		
		
		out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-json", filename_full, "-" + lang ])
		fw2 = open(directoryPrefix + "/out/" + filename_extPrefixStripped + "Intersect.txt", "w")
		fw2.write(out.decode('utf-8'))
