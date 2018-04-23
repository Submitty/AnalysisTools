import subprocess
import sys
import glob


filename1_full = sys.argv[1]
filename2_full = sys.argv[2]
reportFile = sys.argv[3]
lang = sys.argv[4]

end1 = filename1_full.rfind(".") 
end2 = filename2_full.rfind(".") 

filename1_extStripped = filename1_full[:end1]
filename2_extStripped = filename2_full[:end2]
start1 = filename1_extStripped.rfind("/") + 1
start2 = filename2_extStripped.rfind("/") + 1
filename1_extPrefixStripped = filename1_extStripped[start1:]
filename2_extPrefixStripped = filename2_extStripped[start2:]


print("full filenames")
print(filename1_full, filename2_full)
print("filename ext stripped")
print(filename1_extStripped, filename2_extStripped)
print("filename prefix and ext stripped")
print(filename1_extPrefixStripped, filename2_extPrefixStripped)


try:
	subprocess.check_call(["python3", "/usr/local/submitty/SubmittyAnalysisTools/removeTokens.py", filename1_full])
	subprocess.check_call(["python3", "/usr/local/submitty/SubmittyAnalysisTools/removeTokens.py", filename2_full])
except subprocess.CalledProcessError:
	print("error in remove tokens")
	exit()

try:
	subprocess.check_call(["python3", "/usr/local/submitty/SubmittyAnalysisTools/jsonDiff.py", filename1_full, filename2_full, reportFile, lang])
except subprocess.CalledProcessError:
	print("error in json diff")
	exit()

try:
	html1 = subprocess.check_output(["python3", "/usr/local/submitty/SubmittyAnalysisTools/make_tree_interactive.py", filename1_extStripped + "Modified.json"]).decode("utf-8")
	html2 = subprocess.check_output(["python3", "/usr/local/submitty/SubmittyAnalysisTools/make_tree_interactive.py", filename2_extStripped + "Modified.json"]).decode("utf-8")

except subprocess.CalledProcessError:
	print("error in the tree visualization tool")
	exit()

combinedFile = filename1_extPrefixStripped + filename2_extPrefixStripped


fw = open(combinedFile + ".html", "w+")
fw.write(str(html1) + str(html2))
