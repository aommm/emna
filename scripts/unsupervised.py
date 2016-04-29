from sklearn.cluster import KMeans


from db import get_features, get_lemmas, get_classes
from itertools import groupby
from operator import itemgetter
import sys
import sh

n_clusters = None

def run():
  clf = KMeans(n_clusters=n_clusters)
  features,v = get_features()
  # clf.fit(features)
  x = clf.fit_predict(features)
  # v.inverse_transform()
  # print len(x)
  # print features.shape
  # print features[111]
  y = get_lemmas()
  print len(x), len(y)
  xy = zip(x,y)

  getClass = itemgetter(0);
  xy = sorted(xy, key=getClass)
  for key, group in groupby(xy, getClass):
    print "------------------------------------"
    print 'Cluster', key
    for x in group:
      print x[1][2],'\n'
    # print [x for x in group]
    # listOfThings = " and ".join([thing[1] for thing in group])
    # print key + ":  " + listOfThings + "."
  # TODO: get lemma names from hs_lemma_feature, ordered by lemma name. Goto SQL

def run_extract_features(args):
  # Populate SQL db with features for this scheme
  extractFeatures = sh.Command("./scripts/extractFeatures")
  out = extractFeatures(*args)
  print "invoked extractFeatures with args",args


def main():
  global n_clusters
  def fail():
    print('Usage: python unsupervised.py [number_of_clusters] [feature extraction schemes]')
    sys.exit(0)

  if len(sys.argv) == 1: # no args
    n_clusters = 8
    run()

  elif len(sys.argv) == 2: # only n_clusters
    n_clusters = int(sys.argv[1])
    run()

  else:
    n_clusters = int(sys.argv[1])
    run_extract_features(sys.argv[2:])
    run()

if __name__=='__main__':
  main()