import psycopg2
import numpy
import sys
import os

from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_extraction import DictVectorizer
from sklearn.externals import joblib

# Handle arguments
def fail():
  print('Usage: python learn.py [/path/to/classifier.pkl]')
  sys.exit(0)

classifier_path = None
if len(sys.argv) == 1: # default path: emna/data/classifier.pkl
  scriptPath = os.path.dirname(os.path.realpath(__file__))
  classifier_path = os.path.join(scriptPath, os.pardir, 'data', 'classifier.pkl')
  classifier_path = os.path.abspath(classifier_path) # prettify
elif len(sys.argv) == 2:
  classifier_path = sys.argv[1]
else:
  fail()

# Connect to db
try:
  conn = psycopg2.connect("dbname='hipspec' host='localhost'")
except:
  print "I am unable to connect to the database"
  sys.exit(0)


# Create feature matrix
def get_features():
  # Get from db
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma_feature""")
  rows = cur.fetchall()
  # Create dict
  features = dict()
  for [lemma, feature] in rows:
    if not lemma in features:
      features[lemma] = dict()
    features[lemma][feature] = 1 # TODO check if already exists, should keep count?
  # Convert to numerical thingy
  features_list = [features[lemma] for lemma in features]
  v = DictVectorizer()
  features_arr = v.fit_transform(features_list)
  return features_arr


# Create array of classes (i.e. ["[0,1]", "[0]"])
def get_classes():
  # Get from db
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma""")
  rows = cur.fetchall()
  # Create set
  classes = set()
  for [lemma, indvars] in rows:
    clas = str(indvars)
    classes.add(clas)
  # Convert to numerical thingy
  classes_arr = numpy.array(list(classes))
  return classes_arr    


# Trains on features/classes. Returns a classifier
def train(features, classes):
  clf = BernoulliNB()
  clf.fit(features, classes)
  return clf

def __main__():
  print 'getting features from database'
  features = get_features()
  classes = get_classes()
  print 'training'
  clf = train(features, classes)
  print 'saving to file'
  joblib.dump(clf, classifier_path) 
  print 'done!'

  # testPrediction = clf.predict(features[0])
  # print 'predicting first datapoint as belonging to', testPrediction


__main__()