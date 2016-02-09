import sh
import sys
import os
from os.path import isfile, isdir, join, splitext

# Handle arguments
dir_path = sys.argv[1]
if not isdir(dir_path):
	print('Usage: python batch.py /path/to/directory')
	sys.exit(0)
print()

def isproblem(f):
	return isfile(join(dir_path, f)) and splitext(f)[1] == '.smt2'

# Get all files to process
problems = [f for f in os.listdir(dir_path) if isproblem(f)]

# Run emna for each file
successful = 0
total  = 0
for problem in problems:
	total += 1
	problem_name = os.path.splitext(problem)[0]
	problem_file = join(dir_path, problem)
	problem_proof_file = join(dir_path, problem_name + '.proof')
	print("proving "+problem_name)
	# try:
	output = sh.emna('--prover=z', '--output='+problem_proof_file, problem_file, _ok_code=[0,1])
	if output.exit_code == 0:
		successful += 1
		print("... success! ("+str(successful)+"/"+str(total)+")")
	else:
		print(output.exit_code)
		print("... fail ("+str(successful)+"/"+str(total)+")")

