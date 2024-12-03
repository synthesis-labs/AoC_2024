import re

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')


count = 0
for x in data:
    # print(x)
    r = re.findall(r'mul\((\d+),(\d+)\)', x)
    for x in r:
        count += int(x[0]) * int(x[1])

print(count)


#xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

count = 0
include = True
for x in data:
    # print(x)
    r = re.findall(r'(do.+?)?mul\((\d+),(\d+)\)', x)
    print(r)
    for x in r:
        if 'don\'t' in x[0]:
            include = False
        elif 'do' in x[0]:
            include = True
        if include:
            count += int(x[1]) * int(x[2])

print(count)