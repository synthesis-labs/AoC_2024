from itertools import groupby

with open('data.txt', 'r') as f:
    data = f.readlines()

# split data
data = [x.split() for x in data]
list1 = [int(x[0]) for x in data]
list2 = [int(x[1]) for x in data]

# sort
list1.sort()
list2.sort()

distance = sum([abs(list1[i] - list2[i]) for i in range(len(list1))])
print(f"Part 1: {distance}")

# Part 2.

list2_groups = groupby(list2)
list2_dict = {k: len(list(v)) for k, v in list2_groups}
score = 0
for i in list1:
    if i in list2_dict:
        score += i * list2_dict[i]

print(f"Part 2: {score}")

