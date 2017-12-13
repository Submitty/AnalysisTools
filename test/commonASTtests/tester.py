import sys
import subprocess
import glob
import os

print "Python files"
print

for fname in glob.glob("py-test-files/*.py"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-py", fname])

print
print "C++ files"
print

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

'''
for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw1/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw2/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw3/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw4/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])


for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw5/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw6/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])


for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw7/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])



for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw8/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw9/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])

for fname in glob.glob("/mnt/c/Users/Elizabeth/Documents/spring2017/commonASTs/cpp-test-files/dataStructures/hw10/*.cpp"):
	print
	print "RUNNING RUNNER ON ", fname
	subprocess.call(["python", "runner.py", "-cpp", fname])
'''
