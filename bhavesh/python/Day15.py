def pushBoxes(grid, boxesPos, delta):
  (curX, curY) = boxesPos[-1]
  (dx, dy) = delta
  (newX, newY) = (curX + dx, curY + dy)
  if grid[newX][newY] == '.':
    newBoxesPos = [(x + dx, y + dy) for (x, y) in boxesPos]
    for b in newBoxesPos:
      (nbx, nby) = b
      grid[nbx][nby] = 'O'
    (firstX, firstY) = boxesPos[0]
    grid[firstX][firstY] = '.'
    return (grid, True)
  elif grid[newX][newY] == 'O':
    return pushBoxes(grid, boxesPos + [(newX, newY)], delta)
  elif grid[newX][newY] == '#':
    return (grid, False)
  else:
    return (grid, False)


def doAMove(grid, currentPos, move):
  (curX, curY) = currentPos
  (dx, dy) = move
  (newX, newY) = (curX + dx, curY + dy)
  if grid[newX][newY] == '.':
    grid[curX][curY], grid[newX][newY] = grid[newX][newY], grid[curX][curY]
    return grid
  elif grid[newX][newY] == '#':
    return grid
  elif grid[newX][newY] == 'O':
    (grid, changed) = pushBoxes(grid, [(newX, newY)], (dx, dy))
    if changed:
      grid[curX][curY], grid[newX][newY] = grid[newX][newY], grid[curX][curY]
    return grid
  else:
    return grid

def findStart(grid):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == '@':
        return (i, j)


def printGrid(grid):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      print(grid[i][j], end='')
    print()


with open("input.txt", 'r') as infile:
  data = infile.read()

splitData = data.split("\n\n")

grid = [[y for y in x] for x in splitData[0].split("\n")]
moves = [item for row in splitData[1].split("\n") for item in row]

deltas = {
  '^': (-1, 0),
  '>': (0, 1),
  'v': (1, 0),
  '<': (0, -1)
}

largeGrid = []
for i in range(len(grid)):
  newRow = []
  for j in range(len(grid[i])):
    if grid[i][j] == '#':
      newRow.append('#')
      newRow.append('#')
    elif grid[i][j] == 'O':
      newRow.append('[')
      newRow.append(']')
    elif grid[i][j] == '.':
      newRow.append('.')
      newRow.append('.')
    else:
      newRow.append('@')
      newRow.append('.')
  largeGrid.append(newRow)


def part1(grid, moves):
  for move in moves:
    start = findStart(grid)
    grid = doAMove(grid, start, deltas[move])

  part1 = 0
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == 'O':
        part1 += 100 * i + j

  printGrid(grid)
  print(f"Part 1: {part1}")

part1(grid, moves)
