# read data.txt
import re

with open('data.txt', 'r') as f:
    data = f.readlines()

data = "".join(data)

matches =  re.findall(r"mul\(\d{1,3},\d{1,3}\)", data)
# print(matches)

sum = 0
for mul in matches:
    groups = re.match(r"mul\((\d{1,3}),(\d{1,3})\)", mul)
    # print(groups.groups())
    sum += int(groups.groups()[0]) * int(groups.groups()[1])

print(f"Part 1: {sum}")

matches2 = re.findall(r"do\(\)|don't\(\)|mul\(\d{1,3},\d{1,3}\)", data)
# print (matches2)

enabled = True
sum2 = 0
for m in matches2:
    if m == "do()":
        enabled = True
    elif m == "don't()":
        enabled = False
    elif enabled:
        groups = re.match(r"mul\((\d{1,3}),(\d{1,3})\)", m)
        sum2 += int(groups.groups()[0]) * int(groups.groups()[1])

print(f"Part 2: {sum2}")
