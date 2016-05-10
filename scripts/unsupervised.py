from sklearn.cluster import KMeans, Birch, MeanShift

from db import get_features, get_lemmas, get_classes, remove_popular
from itertools import groupby
from operator import itemgetter
import sys
import sh
import numpy

n_clusters = None
commutative_lemmas = ['lemma-100', 'lemma-105', 'lemma-110', 'lemma-189', 'lemma-196', 'lemma-337', 'lemma-347', 'lemma-357', 'lemma-39', 'lemma-427', 'lemma-55', 'lemma-65', 'lemma-95']
associative_lemmas = ['lemma-101', 'lemma-108', 'lemma-24', 'lemma-429', 'lemma-68', 'lemma-91', 'lemma-98']

def run():
  #init=numpy.array([[1,0,0], [0,1,0], [0,0,1]])
  clf = KMeans(n_clusters=n_clusters)
  #clf = Birch(n_clusters = n_clusters)

  features,v,featuresDict = get_features()

  y = get_lemmas()

  #featuresDict = remove_popular(featuresDict, len(y))

  # print "Now we have %i features instead " % (len(featCount.keys()))

  #print v.get_feature_names()
  #print features

  # clf.fit(features)
  x = clf.fit_predict(features)

  # featureDict = v.inverse_transform(features)
  # print len(x)
  # print features.shape
  # print features[111]
  print len(x), len(y)
  xy = zip(x,y)

  getClass = itemgetter(0);
  xy = sorted(xy, key=getClass)
  for key, group in groupby(xy, getClass):
    print "------------------------------------"
    print 'Cluster', key

    groupFeats = dict()
    n = 0

    for x in group:
      #print x
      print x[1][0]
      #print x[1][2]
      n = n + 1

      #if #x[1][0] in featCount:
        #feats = featuresDict[x[1][0]].keys()
        
        #print '\n'

    print "\nCluster summary: "
    print "%i number of lemmas" % (n)

    #if n > 5:
    #  for k in groupFeats.keys():
    #    if groupFeats[k] > n/3:
    #      print k + (" %i" % groupFeats[k]) + (", %i percent" % int((float(groupFeats[k])/n)*100))


    # print [x for x in group]
    # listOfThings = " and ".join([thing[1] for thing in group])
    # print key + ":  " + listOfThings + "."
  # TODO: get lemma names from hs_lemma_feature, ordered by lemma name. Goto SQL

def run_extract_features(args):
  # Populate SQL db with features for this scheme
  extractFeatures = sh.Command("./scripts/extractFeatures")
  out = extractFeatures(*args)
  #print out
  print "invoked extractFeatures with args",args


def main():
  global n_clusters
  def fail():
    print('Usage: python unsupervised.py number_of_clusters path/to/lib.tiplib depth [feature extraction schemes]')
    print('E.g.: python scripts/unsupervised.py 8 ./data/lib.tiplib 4 la ls')
    sys.exit(0)

  if len(sys.argv) < 4: # no args
    fail()

  else:
    n_clusters = int(sys.argv[1])
    run_extract_features(sys.argv[2:])
    run()

if __name__=='__main__':
  main()