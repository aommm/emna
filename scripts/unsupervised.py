from sklearn.cluster import KMeans, Birch, MeanShift

from db import get_features, get_lemmas, get_classes, remove_popular
from itertools import groupby
from operator import itemgetter
import sys
import sh
import numpy

n_clusters = None

def run():
  clf = KMeans(n_clusters=n_clusters)
  features,v,featuresDict = get_features()

  y = get_lemmas()
  print "%i features are used..." % len(v.get_feature_names())

  x = clf.fit_predict(features)
  xy = zip(x,y)
  
  getClass = itemgetter(0);
  xy = sorted(xy, key=getClass)
  for key, group in groupby(xy, getClass):
    print "------------------------------------"
    print 'Cluster', key

    groupFeats = dict()
    n = 0

    for x in group:
      print "%s, %s" % (x[1][0], x[1][2])
      n = n + 1

    print "\nCluster summary: "
    print "%i number of lemmas\n" % (n)

def run_extract_features(args):
  # Populate SQL db with features for this scheme
  extractFeatures = sh.Command("./scripts/extractFeatures")
  out = extractFeatures(*args)
  print out
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