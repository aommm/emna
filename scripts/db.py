
import psycopg2
import sys
import os
from sklearn.feature_extraction import DictVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.feature_extraction.text import CountVectorizer
import numpy
from functools import partial
import nltk

db_name = os.getenv('HS_DB_NAME', 'hipspec')
db_host = os.getenv('HS_DB_HOST', 'localhost')
db_username = os.getenv('HS_DB_USERNAME', None)
db_password = os.getenv('HS_DB_PASSWORD', None)

# Connect to db
try:
  conn_string = "dbname='"+db_name+"' host='"+db_host+"'"
  if db_username is not None:
    conn_string = conn_string + " user='"+db_username+"'"
  if db_password is not None:
    conn_string = conn_string + " password='"+db_password+"'"
  print "connString:"+conn_string
  conn = psycopg2.connect(conn_string)
except:
  print "I am unable to connect to the database"
  sys.exit(0)

# Create feature matrix
def get_features_from_rows(schemes,rows,maxdf):
  features = dict()
  for [lemma, feature, scheme] in rows:
    if scheme in schemes:
      if not lemma in features:
        features[lemma] = dict()
      # print feature
      features[lemma][feature] = 1 # TODO check if already exists, should keep count?

  # Convert to numerical thingy
  #print features
  features_list = [features[lemma] for lemma in features]
  v = DictVectorizer()
  features_arr = v.fit_transform(features_list)

  # Mergeing features into one long string per lemma, separated by #
  concFeats = dict()
  for lemma in features:
    concFeats[lemma] = []
    for f in features[lemma]:
      concFeats[lemma].append(f)

    concFeats[lemma] = "#".join(concFeats[lemma])

  concFeatsList = [concFeats[lemma] for lemma in concFeats]

  vectorizer = TfidfVectorizer(max_df=maxdf,analyzer=partial(nltk.regexp_tokenize, pattern='[^#\s][^\#]*[^#\s]*'))
  countMatrix = vectorizer.fit_transform(concFeatsList)

  return countMatrix, vectorizer

def load_features():
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma_feature ORDER BY lemma""")
  rows = cur.fetchall()
  return rows

# Get all lemmas (same order as get_features())
def get_lemmas():
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma ORDER BY name""")
  rows = cur.fetchall()
  return rows


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

# Create feature matrix
def get_features(maxdf):
  # Get from db
  rows = load_features()
  # Create dict
  features = dict()
  for [lemma, feature, scheme] in rows:
    if not lemma in features:
      features[lemma] = dict()
    features[lemma][feature] = 1
  
  # Mergeing features into one long string per lemma, separated by #
  concFeats = dict()
  for lemma in features:
    concFeats[lemma] = []
    for f in features[lemma]:
      concFeats[lemma].append(f)

    concFeats[lemma] = "#".join(concFeats[lemma])

  concFeatsList = [concFeats[lemma] for lemma in concFeats]

  vectorizer = TfidfVectorizer(max_df=maxdf,analyzer=partial(nltk.regexp_tokenize, pattern='[^#\s][^\#]*[^#\s]*'))
  countMatrix = vectorizer.fit_transform(concFeatsList)

  return countMatrix, vectorizer, features

def remove_popular(featuresDict, nLemmas):

    # Removing super-popular features
  featCount = dict()

  for f in featuresDict:
    for feat in featuresDict[f]:
      if feat not in featCount:
        featCount[feat] = 1
      else:
        featCount[feat] = featCount[feat] + 1

  # print "Currently %i features, before removing popular... " % (len(featCount.keys()))

  keysToRemove = []

  for f in featCount:
    if featCount[f] >= nLemmas/2:
      keysToRemove.append(f)

  for k in keysToRemove:
    for lemma in featuresDict:
      if k in featuresDict[lemma]:
        featuresDict[lemma].pop(k, None)

  return featuresDict

# Get all lemmas (same order as get_features())
def get_lemmas():
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma ORDER BY name""")
  rows = cur.fetchall()
  return rows


# Create array of classes (i.e. ["[0,1]", "[0]"])
def get_classes():
  # Get from db
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_lemma ORDER BY name""")
  rows = cur.fetchall()
  # Extract classes
  classes_list = [str(indvars) for [lemma, indvars, body] in rows]
  # Convert to numerical thingy
  classes_arr = numpy.array(list(classes_list))
  return classes_arr    

