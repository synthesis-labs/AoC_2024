import re
import numpy as np

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

count = 0
for x in data:
    l = np.array([int(i) for i in x.split()])
    # print(l)
    if (all(np.diff(l) > 0) or all(np.diff(l) < 0)) and (np.max(np.abs(np.diff(l))) <= 3):
        print(l)
        count += 1

print('-------')
count = 0
for x in data:
    l = np.array([int(i) for i in x.split()])
    for p in range(len(l)):
        q = np.delete(l, p)
        if (all(np.diff(q) > 0) or all(np.diff(q) < 0)) and (np.max(np.abs(np.diff(q))) <= 3):
            print(l)
            count += 1
            break
