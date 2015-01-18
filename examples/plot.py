import pylab
import sys

for fname in sys.argv[1:]:
  sine = []
  with open(fname) as f:
    for line in f:
      cols = line.strip().split("=>")
      if len(cols) != 2:
        raise Exception('line {} malformed'.format(line))
      sine.append(float(cols[1]))
  print sine
  pylab.plot(sine)
pylab.show()

