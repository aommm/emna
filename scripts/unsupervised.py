from sklearn.cluster import KMeans

from learn import get_features, get_lemmas, get_classes
from itertools import groupby
from operator import itemgetter

def main():
  clf = KMeans(n_clusters=8)
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

if __name__=='__main__':
  main()