import psycopg2
import numpy
import sys
import os

from sklearn.naive_bayes import BernoulliNB
from sklearn.feature_extraction import DictVectorizer
from sklearn.externals import joblib

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

db_name = os.getenv('HS_DB_NAME', 'hipspec')
db_host = os.getenv('HS_DB_HOST', 'localhost')
db_username = os.getenv('HS_DB_USERNAME', None)
db_password = os.getenv('HS_DB_PASSWORD', None)

# Connect to db
try:
  conn_string = "dbname='"+db_name+"' host='"+db_host+"'"
  if db_username is not None:
    conn_string = conn_string + " username='"+db_username+"'"
  if db_password is not None:
    conn_string = conn_string + " password='"+db_password+"'"
  print "connString:"+conn_string
  conn = psycopg2.connect(conn_string)
except:
  print "I am unable to connect to the database"
  sys.exit(0)


# Create feature matrix
def get_features():
  # Get from db
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma_feature ORDER BY lemma""")
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
  return features_arr, v


# Create array of classes (i.e. ["[0,1]", "[0]"])
def get_classes():
  # Get from db
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma ORDER BY name""")
  rows = cur.fetchall()
  # Extract classes
  classes_list = [str(indvars) for [lemma, indvars] in rows]
  # Convert to numerical thingy
  classes_arr = numpy.array(list(classes_list))
  return classes_arr    


# Trains on features/classes. Returns a classifier
def train(features, classes):
  clf = BernoulliNB()
  clf.fit(features, classes)
  return clf

def __main__():
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


__main__()