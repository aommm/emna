
import psycopg2
import sys
import os
from sklearn.feature_extraction import DictVectorizer
import numpy

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
  return features_arr, v, features

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

