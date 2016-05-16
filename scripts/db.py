
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
def get_features_from_rows(schemes,rows,maxdf,binary, tfidf):
  features = dict()
  for [lemma, feature, scheme] in rows:
    if not lemma in features:
      features[lemma] = dict()
    features[lemma][feature] = 1
  
  if tfidf:
    # Mergeing features into one long string per lemma, separated by #
    concFeats = dict()
    for lemma in features:
      concFeats[lemma] = []
      for f in features[lemma]:
        concFeats[lemma].append(f)


      concFeats[lemma] = "#".join(concFeats[lemma])

    concFeatsList = [concFeats[lemma] for lemma in concFeats]

    vectorizer = TfidfVectorizer(binary=binary,max_df=maxdf,analyzer=partial(nltk.regexp_tokenize, pattern='[^#\s][^\#]*[^#\s]*'))
    countMatrix = vectorizer.fit_transform(concFeatsList)

    return countMatrix, vectorizer
  else:
    features_arr = [features[lemma] for lemma in features]
    v = DictVectorizer()
    matrix = v.fit_transform(features_arr)

    return matrix, v

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

  vectorizer = TfidfVectorizer(analyzer=partial(nltk.regexp_tokenize, pattern='[^#\s][^\#]*[^#\s]*'))
  countMatrix = vectorizer.fit_transform(concFeatsList)

#  for i,name in enumerate(vectorizer.get_feature_names()):
#    print "%s, %f" % (name, vectorizer.idf_[i])

  return countMatrix, vectorizer, features

# Create feature matrix
def get_function_features():
  cur = conn.cursor()
  cur.execute("""SELECT * from hs_function_feature ORDER BY function""")
  rows = cur.fetchall()
  # Get from db
  # rows = load_features()
  keys = []
  # Create dict
  features = dict()
  for [function, feature, scheme] in rows:
    if not function in features:
      features[function] = dict()
    features[function][feature] = 1
    if not function in keys:
      keys.append(function)
  
  # Mergeing features into one long string per function, separated by #
  concFeats = dict()
  for function in features:
    concFeats[function] = []
    for f in features[function]:
      concFeats[function].append(f)


    concFeats[function] = "#".join(concFeats[function])

  concFeatsList = [concFeats[function] for function in concFeats]

  print concFeatsList

  vectorizer = TfidfVectorizer(analyzer=partial(nltk.regexp_tokenize, pattern='[^#\s][^\#]*[^#\s]*'))
  countMatrix = vectorizer.fit_transform(concFeatsList)

#  for i,name in enumerate(vectorizer.get_feature_names()):
#    print "%s, %f" % (name, vectorizer.idf_[i])

  return countMatrix, vectorizer, features, keys


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

