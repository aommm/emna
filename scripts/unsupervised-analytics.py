from sklearn.cluster import KMeans, Birch, MeanShift

from db import get_features, get_lemmas, get_classes, remove_popular, get_features_from_rows, load_features
import itertools
from operator import itemgetter
import sys
import sh
import numpy

def run(rows, schemes, maxdf, n_clusters, y):
  #init=numpy.array([[1,0,0], [0,1,0], [0,0,1]])
  clf = KMeans(n_clusters=n_clusters)
  #clf = Birch(n_clusters = n_clusters)

  features,v = get_features_from_rows(schemes, rows, maxdf)

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
  # print len(x), len(y)
  xy = zip(x,y)

  lemmasPerCluster = dict()

  getClass = itemgetter(0);
  xy = sorted(xy, key=getClass)
  for key, group in itertools.groupby(xy, getClass):
    lemmasPerCluster[key] = []

    for x in group:
      lemmasPerCluster[key] = lemmasPerCluster[key] + [x[1][0]]

  return lemmasPerCluster

def analyzeCluster(clusters, maxdf, inputclusters, schemes):
  associative_clusters = []
  commutative_clusters = []

  #print clusters

  for key in clusters:
    for lemma in clusters[key]:
      if lemma in associative_lemmas:
        associative_clusters = associative_clusters + [key]
      if lemma in commutative_lemmas:
        commutative_clusters = commutative_clusters + [key]
  
  # Lemma coherence is how much the lemmas belong to the same cluster. The number of lemmas in the most popular group / total lemmas
  # Lemma uniqueness is how much the most popular cluster of a lemma category belongs only to that lemma

  associative_coherence = get_coherence(associative_clusters)
  commutative_coherence = get_coherence(commutative_clusters)
  
  associative_uniqueness = get_uniqueness(associative_clusters, clusters)
  commutative_uniqueness = get_uniqueness(commutative_clusters, clusters)

  best_uni = None

  if associative_uniqueness > commutative_uniqueness:
    best_uni = associative_uniqueness
  else:
    best_uni = commutative_uniqueness

  return [{ 'best_uni': best_uni, 'maxdf': maxdf, 'schemes': schemes, 'nclusters': (len(clusters.keys())), 'inputclusters': inputclusters, 'ass_coh': associative_coherence, 'ass_uni': associative_uniqueness, 'com_coh': commutative_coherence, 'com_uni': commutative_uniqueness }]


def get_coherence(lemmaClusters):
  popularities = dict()
  for c in lemmaClusters:
    if c not in popularities:
      popularities[c] = 1.0
    else:
      popularities[c] = popularities[c] + 1.0

  popularN = 0.0
  for p in popularities:
    if popularN < popularities[p]:
      popularN = popularities[p]

  return popularN / len(lemmaClusters)

def get_uniqueness(lemmaClusters, totalClusters):
  popularities = dict()
  for c in lemmaClusters:
    if c not in popularities:
      popularities[c] = 1.0
    else:
      popularities[c] = popularities[c] + 1.0

  popularN = 0.0
  popularP = -1

  for p in popularities:
    if popularP == -1:
      popularP = p
      popularN = popularities[p]
    elif popularN < popularities[p]:
      popularP = p
      popularN = popularities[p]

  return popularN / len(totalClusters[popularP])

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

  all_schemes = "fa fs la ls ala afa afs"
  scheme_combos = ["","fa"], ["","fs"], ["","la"], ["", "ls"], ["","ala"], ["","afa"], ["","afs"]
  depths = range(1,4)
  scheme_permutations = list(itertools.product(*scheme_combos))
  dfs = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
  clusters = [32]

  print "Doing %i combinations " % (len(scheme_permutations)*len(dfs)*len(clusters)*len(depths))

  results = []

  for de in depths:
    run_extract_features(['./data/lib.tiplib', de, "fa", "fs", "la", "ls", "ala", "afa", "afs"])
    rows = load_features()
    y = get_lemmas()
    print "de %i" % de
    n = 0
    for d in dfs:
      print "df %i" % d
      for c in clusters:
        print "cluster %i" % c
        for s in scheme_permutations:
          print s
          schemes = [arg for arg in s if arg <> ""]
          if len(schemes) > 0:
            try:
              cluster = run(rows, s, d, c, y)
              results = results + analyzeCluster(cluster, d, c, schemes)
            except ValueError:
              print "Combination resulted in no features... at n = %i" % (n) 
            n = n + 1
    print "%i combinations completed..." % (n)

  results_sorted = sorted(results, key=itemgetter("best_uni"))

  for r in results_sorted:
    print r

if __name__=='__main__':
  main()