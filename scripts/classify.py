import sys
import os

from sklearn.naive_bayes import BernoulliNB
from sklearn.externals import joblib


# Handle arguments
def fail():
  print('Usage: python classify.py formulaOrFeaturesStringifiedInAnAsOfYetUnspecifiedManner [/path/to/classifier.pkl]')
  sys.exit(0)

classifier_path = None
formula_string = None
if len(sys.argv) == 1:
  fail()
if len(sys.argv) == 2:
  formula_string = sys.argv[1]
  # default path: emna/data/classifier.pkl
  scriptPath = os.path.dirname(os.path.realpath(__file__))
  classifier_path = os.path.join(scriptPath, os.pardir, 'data', 'classifier.pkl')
  classifier_path = os.path.abspath(classifier_path) # prettify
elif len(sys.argv) == 3:
  formula_string = sys.argv[1]
  classifier_path = sys.argv[2]
else:
  fail()


def parse_formula(string):
  # TODO implement. parse into features  
  return string

def classify_formula(formula, clf):
  return clf.predict(formula)
  

def __main__():
  formula = parse_formula(formula_string)
  clf = joblib.load(classifier_path)
  clas = classify_formula(formula, clf)
  print clas

__main__()