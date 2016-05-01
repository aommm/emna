import sys
import os

from sklearn.naive_bayes import BernoulliNB
from sklearn.externals import joblib

from db import get_features, get_lemmas, get_classes

# Handle arguments
def fail():
  print('Usage: python learn.py [/path/to/data]')
  sys.exit(0)

data_path = None
if len(sys.argv) == 1: # default path: emna/data
  script_path = os.path.dirname(os.path.realpath(__file__))
  data_path = os.path.join(script_path, os.pardir, 'data') 
elif len(sys.argv) == 2:
  data_path = sys.argv[1]
else:
  fail()
classifier_path = os.path.join(data_path, 'classifier.pkl') 
vectorizer_path = os.path.join(data_path, 'vectorizer.pkl') 
classifier_path = os.path.abspath(classifier_path) # prettify
vectorizer_path = os.path.abspath(vectorizer_path)

# Trains on features/classes. Returns a classifier
def train(features, classes):
  clf = BernoulliNB()
  clf.fit(features, classes)
  return clf

def main():
  print 'getting features from database'
  features, v = get_features()
  classes = get_classes()
  print 'training'
  clf = train(features, classes)
  print 'saving to file'
  joblib.dump(clf, classifier_path) 
  joblib.dump(v, vectorizer_path) 
  print 'done!'

  # testPrediction = clf.predict(features[0])
  # print 'predicting first datapoint as belonging to', testPrediction

if __name__ == '__main__':
  main()