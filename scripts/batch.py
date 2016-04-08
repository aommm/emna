import sh
import sys
import os
import re
import json
import timeit
from os.path import isfile, isdir, join, splitext

# Handle arguments
def fail():
	print('Usage: python batch.py /path/to/directory /path/to/library.lib')
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
	err = err + '\n' + s
def print_out(s):
	global out
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
def calculate_indvar_success(attempt):
	# regex = 'Considering:\n\n(.*)\n\ninduction order from script:\[(\[[0-9]*\]).*\n\nProved by induction on ([a-zA-Z]*)'
	regex = 'Considering:\n\n(.*)\n\ninduction order from script:\[(?:\[(?:\"([a-zA-Z]*)\")*\]).*\n\nProved by induction on ([a-zA-Z]*)'
	matches = re.findall(regex, attempt['stdout'])
	n_correct = 0
	n_total = 0
	for match in matches:
		n_total += 1
		if match[1] == match[2]:
			n_correct += 1
	attempt['lemma_indvar_total'] = n_total
	attempt['lemma_indvar_correct'] =  n_correct


# Run emna for each file
for problem in problems:
	total += 1
	problem_name = os.path.splitext(problem)[0]
	problem_file = join(dir_path, problem)
	print("proving "+problem_name)
	# Run emna
	start_time = timeit.default_timer()
	output = sh.emna('--prover=z', '--output='+proof_path, problem_file, _ok_code=[0,1], _err=print_err, _out=print_out, _tty_out=False)
	elapsed = timeit.default_timer() - start_time
	# Check result
	isOk = output.exit_code == 0
	if isOk:
		successful += 1
		print("... success! ("+str(successful)+"/"+str(total)+")")
	else:
		print("... fail ("+str(successful)+"/"+str(total)+")")
	# Save result to file and reset
	logFile = open('./batch.log', 'w')
	timestamp = int(timeit.default_timer())*1000
	attempt = {'name': problem_name, 'dir': dir_path, 'ok': isOk, 'elapsed': elapsed, 'timestamp': timestamp, 'stdout': out, 'stderr': err}
	calculate_indvar_success(attempt)
	outputList.append(attempt)
	outputStr = json.dumps(outputList, sort_keys=True, indent=2, separators=(',', ': '))
	print >>logFile, outputStr
	logFile.close()
	err = ''
	out = ''


