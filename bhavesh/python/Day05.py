import itertools

with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

rules = {}
updates = []
for line in data:
  if line == "":
    continue
  elif "|" in line:
    rule = line.split("|")
    if not int(rule[0]) in rules:
      rules[int(rule[0])] = [int(rule[1])]
    else:
      rules[int(rule[0])].append(int(rule[1]))
  else:
    updates.append([int(x) for x in line.split(",")])

def processUpdate(update):
  for i in range(len(update)):
    n = update[i]
    if n not in rules:
      continue
    for b in update[:i]:
      if b in rules[n]:
        return False, update
  return True, update

def processUpdate2(update):
  i = 0
  while i < len(update):    
    n = update[i]
    if n not in rules:
      i += 1
      continue
    didAnySwap = False
    for j in range(len(update[:i])):
      b = update[j]
      if b in rules[n]:
        update[i], update[j] = update[j], update[i]
        didAnySwap = True
        break
    if didAnySwap:
      i = 0
    else:
      i += 1
  return update

def getMiddle(update):
  return update[len(update) // 2]

correctlyOrdered = [update for update in updates if processUpdate(update)[0] == True]
incorrectlyOrdered = [update for update in updates if processUpdate(update)[0] == False]

sumCorrectlyOrdered = sum([getMiddle(update) for update in correctlyOrdered])
print("Part 1:", sumCorrectlyOrdered)

sumCorrectedIncorrectlyOrdered = sum([getMiddle(processUpdate2(update) ) for update in incorrectlyOrdered])
print("Part 2:", sumCorrectedIncorrectlyOrdered)


