import re
import numpy as np

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

data = [x.split() for x in data]
l = np.array(data).astype(int)

l1 = np.sort(l[:,0])
l2 = np.sort(l[:,1])

d = [np.abs(y-x) for x,y in zip(l1, l2)]

print(np.sum(d))

c = [x * np.sum(l2 == x) for x in l1]

print(np.sum(c))