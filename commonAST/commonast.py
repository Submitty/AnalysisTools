#!/usr/bin/env python3

import subprocess
import sys

#To do:
#	more than one file?
#	fix python errors
#	fix why its running python
#	documentation?

lang = sys.argv[1]
filename = sys.argv[2]

if lang == "-py":
	subprocess.call(["python", "astMatcher.py", filename])
elif lang == "-cpp":
	f = open("out.txt", "w")
	alias = "astMatcherCpp " + filename
	p = subprocess.Popen(["/bin/bash", "-i", "-c", alias], stdout=f)
	(out,err) = p.communicate()
	if p.returncode != 0 and p.returncode <= 125:
		print ("command failed, exit-code=%d error = %s" % (p.returncode, str(err)))
	elif p.returncode == 127:
        	print ("program not found: %s" % (str(err)))
else:
	print "invalid language"
	#sys.exit();

subprocess.call(["./count.out", "out.txt", "-Complexity", "void"])
