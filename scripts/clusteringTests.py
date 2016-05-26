import os

def main():
  # Loop over all possible feature extraction schemes
  # TODO: any point with depth=0?

  schemes = ["fs","fa"]
  depths= [1,2,3]
  clusters = [2,8,20]
  n = len(depths)*len(clusters)*len(schemes)

  print "Processing %i combinations for clustering" % n
  
  os.system("rm ./clustering/*")

  for s in schemes:
    for d in depths:
      for c in clusters:
        for n in range(0,10):
          os.system("python ./scripts/unsupervised.py %i ./data/library.tiplib %i %s >> ./clustering/%s_d%i_c%i.txt" % (c,d,s,s,d,c))
          if n < 9:
            os.system("echo \"####################################################################\n\" >> ./clustering/%s_d%i_c%i.txt " % (s,d,c))

if __name__ == '__main__':
  main()