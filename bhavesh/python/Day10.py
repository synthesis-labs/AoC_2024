with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

grid = [[int(y) for y in x] for x in data]

def findPaths(grid, start, end, path=None, visited=None):
  rows, cols = len(grid), len(grid[0])
  x, y = start
  path = path or []
  visited = visited or set()

  path.append((x, y))
  visited.add((x, y))

  if start == end:
    return [list(path)]

  all_paths = []
  current_value = grid[x][y]

  directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
  for dx, dy in directions:
    nx, ny = x + dx, y + dy
    if 0 <= nx < rows and 0 <= ny < cols and (nx, ny) not in visited:
      next_value = grid[nx][ny]
      if next_value - current_value == 1:
        all_paths.extend(
          findPaths(grid, (nx, ny), end, list(path), set(visited)))

  return all_paths


starts, dests = [], []
for i in range(len(grid)):
  for j in range(len(grid[i])):
    val = grid[i][j]
    if (val == 0):
      starts.append((i, j))
    if (val == 9):
      dests.append((i, j))

part1 = 0
part2 = 0
for start in starts:
  pathsForStart = [findPaths(grid, start, d) for d in dests]
  numTrailheads = len([p for p in pathsForStart if len(p) > 0])
  trailHeadRating = sum([len(p) for p in pathsForStart])
  part1 += numTrailheads
  part2 += trailHeadRating

print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
