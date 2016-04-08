import sh
import sys
import re
import os
import json
import timeit
from os.path import isfile, isdir, join, splitext

# Evaluates the log file by reading stdout, and figuring out how many lemmas
# were proven by the induction variable suggested by

# NOTE: This file is outdated atm, this is integrated in batch.py instead!

def evaluate_attempt(attempt):
	# regex = 'Considering:\n\n(.*)\n\ninduction order from script:\[(\[[0-9]*\]).*\n\nProved by induction on ([a-zA-Z]*)'
	regex = 'Considering:\n\n(.*)\n\ninduction order from script:\[(?:\[(?:\"([a-zA-Z]*)\")*\]).*\n\nProved by induction on ([a-zA-Z]*)'
	matches = re.findall(regex, attempt['stdout'])
	n_correct = 0
	n_total = 0
	for match in matches:
		print match
		n_total += 1
		if match[1] == match[2]:
			n_correct += 1
	print n_correct, n_total


def main():
	logFile = open('./batch.log', 'r')
	attempts = json.load(logFile)
	logFile.close()
	attempts = [evaluate_attempt(attempt) for attempt in attempts]

	evaluate_attempt


if __name__ == "__main__":
	main()