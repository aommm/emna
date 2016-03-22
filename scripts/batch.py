import sh
import sys
import os
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

# Run emna for each file
for problem in problems:
	total += 1
	problem_name = os.path.splitext(problem)[0]
	problem_file = join(dir_path, problem)
	print("proving "+problem_name+str(timeit.default_timer()))
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
	outputList.append({'name': problem_name, 'dir': dir_path, 'ok': isOk, 'elapsed': elapsed, 'timestamp': timestamp, 'stdout': out, 'stderr': err})
	outputStr = json.dumps(outputList, sort_keys=True, indent=2, separators=(',', ': '))
	print >>logFile, outputStr
	logFile.close()
	err = ''
	out = ''


