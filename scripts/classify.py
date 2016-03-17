import sys
import os

import numpy
from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_extraction import DictVectorizer
from sklearn.externals import joblib


# Handle arguments
def fail():
  print('Usage: python classify.py formulaOrFeaturesStringifiedInAnAsOfYetUnspecifiedManner [/path/to/data]')
  sys.exit(0)

data_path = None
formula_string = None
if len(sys.argv) == 1:
  fail()
if len(sys.argv) == 2:
  formula_string = sys.argv[1]
  # default path: emna/data.pkl
  scriptPath = os.path.dirname(os.path.realpath(__file__))
  data_path = os.path.join(scriptPath, os.pardir, 'data')
elif len(sys.argv) == 3:
  formula_string = sys.argv[1]
  data_path = sys.argv[2]
else:
  fail()
classifier_path = os.path.join(data_path, 'classifier.pkl') 
vectorizer_path = os.path.join(data_path, 'vectorizer.pkl') 
classifier_path = os.path.abspath(classifier_path) # prettify
vectorizer_path = os.path.abspath(vectorizer_path)

def parse_formula(string, v):
  # TODO implement. parse into features  
  features = string.split(',')
  features_dict = dict()
  for feature in features:
    features_dict[feature] = 1

  # Convert to numerical thingy
  features_list = [features_dict]
  features_arr = v.transform(features_list)


  return features_arr

def classify_formula(formula, clf):
  return clf.predict(formula)
  

def __main__():
  clf = joblib.load(classifier_path)
  v = joblib.load(vectorizer_path)
  formula = parse_formula(formula_string, v)
  clas = classify_formula(formula, clf)
  # TODO: return all classes, sorted by probability (i.e. [[0],[1],[]] etc)
  # TODO: parse 'clas' into real array, can then print arrayofarrays without quotes
  classes = numpy.array(clas)
  sys.stdout.write(numpy.array_str(classes))

__main__()