import sys
import os
import ast
import numpy
from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_extraction import DictVectorizer
from sklearn.externals import joblib


# Handle arguments
def fail():
  print('Usage: python classify.py formulaOrFeaturesStringifiedInAnAsOfYetUnspecifiedManner [/path/to/data]')
  sys.exit(0)

data_path = None
features_string = None
if len(sys.argv) == 1:
  fail()
if len(sys.argv) == 2:
  features_string = sys.argv[1]
  # default path: emna/data.pkl
  scriptPath = os.path.dirname(os.path.realpath(__file__))
  data_path = os.path.join(scriptPath, os.pardir, 'data')
elif len(sys.argv) == 3:
  features_string = sys.argv[1]
  data_path = sys.argv[2]
else:
  fail()
classifier_path = os.path.join(data_path, 'classifier.pkl') 
vectorizer_path = os.path.join(data_path, 'vectorizer.pkl') 
classifier_path = os.path.abspath(classifier_path) # prettify
vectorizer_path = os.path.abspath(vectorizer_path)

# parse string of features list into list-of-list-of-feature vectors
# E.g. "['++','List']" -> [[ scikit-learn magical numbers ]]
# @param {String} string
# @param {DictVectorizer} v 
def parse_features(string, v):
  features = ast.literal_eval(string)
  features_dict = dict()
  for feature in features:
    features_dict[feature] = 1
  # Convert to numerical thingy
  features_list = [features_dict]
  features_arr = v.transform(features_list)
  return features_arr

def classify_features(formula, clf):
  probs = clf.predict_proba(formula)[0]
  probs_classes = zip(probs, clf.classes_)
  probs_classes.sort(key=lambda x: x[0], reverse=True) # sort by probability
  classes = [ast.literal_eval(x[1]) for x in probs_classes] # convert i.e. (0.5,'[0]') to [0]
  return classes
  
def __main__():
  try:
    clf = joblib.load(classifier_path)
    v = joblib.load(vectorizer_path)
    features = parse_features(features_string, v)
    classes = classify_features(features, clf)
    sys.stdout.write(str(classes)) # print without newline
  except Exception:
    sys.stdout.write("[]")

__main__()