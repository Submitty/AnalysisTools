import subprocess
import sys
import glob
import json
import os

inputdir = sys.argv[1]
outputdir = sys.argv[2]
lang = sys.argv[3]

assignmentName = inputdir[inputdir.rfind("/")+1:]


subdirs =  [name for name in os.listdir(inputdir)
            if os.path.isdir(os.path.join(inputdir, name))]	


sourceFiles = []

#subdirs are student folders
for subdir in subdirs:
	#if not os.path.isdir(inputdir + subdir + "/user_assignment_settings.json"): continue
	
	try:
		jsonFile = open(inputdir + subdir + "/user_assignment_settings.json")
	except:
		continue

	assignmentSettings = json.load(jsonFile)
	nextFolder = str(assignmentSettings["active_version"])


	for root, dirs, files in os.walk(os.path.join(inputdir, subdir, nextFolder), topdown=False):
		for fname in files:
			if not fname.endswith("." + lang): continue
			
			#sourceFile = (inputdir + subdir + "/" + nextFolder + "/*." + lang)
			sourceFiles.append(os.path.join(root, fname))

for source in sourceFiles:	
	print(source)
	for fname in glob.glob(source):
		print(fname)
		if lang == "py":
			out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/diagnostics", "-l", "python", fname])
		elif lang == "cpp":
			out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/unionToolRunner.py", "-json", fname])
		else:
			print("Language not supported. Enter py or cpp")
			exit(1)

		index1 = source.find("/")+1
		index2 = source.find("/", index1)
		studentName = source[index1:index2]
		

		fw = open(outputdir + "/" + studentName + "_" + assignmentName + "Union.txt", "w")
		fw.write(out.decode('utf-8'))

		try:
			out = subprocess.check_output(["/usr/local/submitty/SubmittyAnalysisTools/commonast.py", "-json", fname, "-" + lang ])
		except subprocess.CalledProcessError:
			print("error in commonast")

		fw2 = open(outputdir + "/" + studentName + "_" + assignmentName + "Intersect.txt", "w")
		fw2.write(out.decode('utf-8'))

try:
	subprocess.check_call(["python3", "/usr/local/submitty/SubmittyAnalysisTools/jsonDiffRunnerRunner.py", outputdir, lang])
except subprocess.CalledProcessError:
	print("error in json diff runner runner")
