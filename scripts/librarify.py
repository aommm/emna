import sh
import sys
import os
import re
import json
import timeit
from os.path import isfile, isdir, join, splitext

# Handle arguments
def fail():
	print('Usage: python librarify.py /path/to/directory /path/to/library.lib')
	sys.exit(0)
if len(sys.argv) != 3: fail()
dir_path = sys.argv[1]
proof_path = sys.argv[2]
if not (isdir(dir_path) and (not isdir(proof_path))): fail()

def isproblem(f):
	return isfile(join(dir_path, f)) and splitext(f)[1] == '.smt2'
 
# Error logs
err = ''
out = ''
def print_err(s):
	global err
	s = s.replace('\b', '')
	err = err + '\n' + s
def print_out(s):
	global out
	s = s.replace('\b', '')
	out = out + '\n' + s

# Get all files to process
problems = [f for f in os.listdir(dir_path) if isproblem(f)]

# Statistics
successful = 0
total  = 0
outputList = []
try:
	logFile = open('./batch.log', 'r')
	outputList = json.load(logFile)
	logFile.close()
except:
	pass

# Calculates how many of the lemmas were proven by the ML-suggested induction variable
# Adds attempt['lemma_indvar_correct'] and attempt['lemma_indvar_total']
# (Doesn't count lemmas that don't use induction)
# Removes trailing slash from string s, if any. Returns the new string
def remove_trailing_slash(s):
	if len(s)==0:
		return s
	if s[len(s)-1]=='/':
		return s[0:-1]
	return s

# Run for each file
for problem in problems:
	total += 1
	problem_name = os.path.splitext(problem)[0]
	problem_file = join(dir_path, problem)
	# If this dir/problem is present in batch.log, skip it
	already_done = [item for item in outputList if remove_trailing_slash(item['dir']) == remove_trailing_slash(dir_path) and item['name'] == problem_name]
	if len(already_done)>0 and already_done[0]['ok']:
		print "skipping "+problem_name
		continue
	print("proving "+problem_name)
	# Run
	args = ['--output='+proof_path, problem_file]
	librarify = sh.Command("./librarify")
  	output = librarify(*args)
  	print output
	# Check result
	# Save result to file and reset
	logFile = open('./librarify.log', 'w')
	print >>logFile
	logFile.close()
	err = ''
	out = ''


