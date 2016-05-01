import numpy
import sys
import os
import itertools
import sh
import operator

from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_extraction import DictVectorizer
from sklearn import cross_validation

from oldlearn import get_features, get_classes

def compute_score():
  """Computes the score for the features currently in the database using cross-validation"""
  clf = BernoulliNB()
  features, v = get_features()
  classes = get_classes()
  result = cross_validation.cross_val_score(clf, features, classes, cv=5)
  return result

def process_combination(args,i,n):
  print "%i/%i" % (i,n)
  # remove ""
  args = [arg for arg in args if arg <> ""]
  # If no feature extraction schemes, abort
  if len(args) < 3:
    return False
  # Populate SQL db with features for this scheme
  extractFeatures = sh.Command("./scripts/extractFeatures")
  out = extractFeatures(*args)
  print "invoked extractFeatures with args",args
  # Compute how good it was
  scores = compute_score()
  # print "Running with arguments", args
  # print scores
  # return scores
  return {"args": args, "mean": scores.mean(), "deviation": scores.std()*2}


def main():
  # Loop over all possible feature extraction schemes
  # TODO: any point with depth=0?
  all_args = [["./data/lib.tiplib"], range(2,3),
    ["","fa"], ["","fs"], ["","la"], ["", "ls"]]
  # all_args = [["./data/lib.tiplib"], range(1,5), ["","fa"], ["","fs"], ["","la"], ["", "ls"]]
  arg_combinations = list(itertools.product(*all_args))
  n = len(arg_combinations)
  print "Evaluating %i feature extraction schemes..." % n
  results = [process_combination(args,i,n) for i,args in enumerate(arg_combinations)]

  print results

  # results = [process_combination(arg_combinations[100],1,2), process_combination(arg_combinations[10],2,2)]
  results = [result for result in results if result <> False] # remove False values
  results_sorted = sorted(results, key=operator.itemgetter("mean"))
  print ""
  print "Index\tAverage score\t\tFeature extraction arguments"
  for i,result in enumerate(results_sorted):
    nice_str = "%i.\t%0.2f (+/- %0.2f)\t\t" % (i, result['mean'], result['deviation'])
    print nice_str, result['args']

if __name__ == '__main__':
  main()
