from heapq import heappop, heappush


def findPaths(grid, start, end):
  """
  A* pathfinding algorithm that penalizes turns in the path.

  Args:
      grid (list[list[int]]): 2D grid where '.' = passable and '#' = obstacle.
      start (tuple[int, int]): Start coordinate (row, col).
      end (tuple[int, int]): End coordinate (row, col).

  Returns:
      list[tuple[int, int, str]]: Path with coordinates and directions.
  """
  rows, cols = len(grid), len(grid[0])
  directions = [
      (0, 1, '>'),   # Right
      (1, 0, 'v'),   # Down
      (0, -1, '<'),  # Left
      (-1, 0, '^')   # Up
  ]

  def is_valid(r, c):
    return 0 <= r < rows and 0 <= c < cols and grid[r][c] != "#"

  # Priority queue: stores (cost, row, col, direction, path)
  pq = [(0, start[0], start[1], '>', [(start[0], start[1], '>')])]
  visited = {}
  paths = []  # Store all valid paths to the end

  while pq:
    cost, r, c, prev_dir, path = heappop(pq)

    # If we reach the end, return the path
    if (r, c) == end:
      paths.append(path[:-1])
      continue

    # Mark as visited with the lowest cost
    if (r, c) in visited and visited[(r, c)] <= cost:
      continue
    visited[(r, c)] = cost

    # Explore all possible directions
    for dr, dc, d in directions:
      nr, nc = r + dr, c + dc

      if is_valid(nr, nc):
        # Add the new node to the path
        new_path = path + [(nr, nc, d)]

        # Base cost + turn penalty
        new_cost = cost
        if d != prev_dir:
          new_cost = cost + 1001
        else:
          new_cost = cost + 1

        heappush(pq, (new_cost, nr, nc, d, new_path))

  return paths


def findItem(grid, item):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      if grid[i][j] == item:
        return (i, j)


def printGrid(grid):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      print(grid[i][j], end='')
    print()


def plotPathInGrid(grid, path):
  g = [[y for y in x] for x in grid]
  for (r, c, d) in path:
    if (g[r][c] == 'E' or g[r][c] == 'S'):
      continue
    g[r][c] = d

  printGrid(g)
  print()


def calculatePathCost(path):
  cost = 0
  currentDir = ">"

  for (r, c, d) in path:
    if d != currentDir:
      cost += 1001
    else:
      cost += 1
    currentDir = d

  return cost


with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

grid = [[y for y in x] for x in data]
start = findItem(grid, "S")
end = findItem(grid, "E")
paths = findPaths(grid, start, end)

print(len(paths))

for p in paths:
  plotPathInGrid(grid, p)
  print(calculatePathCost(p), p)
  print()
