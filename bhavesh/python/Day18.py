from heapq import heappop, heappush


def a_star(grid, start, end):
  cols, rows = len(grid), len(grid[0])
  directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # Down, right, up, left

  # Heuristic: Manhattan distance
  def heuristic(x, y):
    return abs(x - end[0]) + abs(y - end[1])

  # Priority queue for open set
  open_set = []
  heappush(open_set, (0, start))  # (priority, (x, y))

  # Maps to store costs
  g_score = {start: 0}  # Cost to reach a node
  came_from = {}  # To reconstruct the path

  while open_set:
    _, current = heappop(open_set)

    # If we reach the destination, reconstruct and return the path
    if current == end:
      return reconstruct_path(came_from, current)

    y, x = current

    for dx, dy in directions:
      nx, ny = x + dy, y + dx

      # Check bounds and walkable cells
      if 0 <= nx < cols and 0 <= ny < rows and grid[ny][nx] != "#":
        tentative_g_score = g_score[current] + 1  # Cost of 1 per step

        neighbor = (ny, nx)
        if tentative_g_score < g_score.get(neighbor, float('inf')):
          # Update best path to neighbor
          came_from[neighbor] = current
          g_score[neighbor] = tentative_g_score
          f_score = tentative_g_score + heuristic(nx, ny)
          heappush(open_set, (f_score, neighbor))

  return []  # No path found


def reconstruct_path(came_from, current):
  path = [current]
  while current in came_from:
    current = came_from[current]
    path.append(current)
  path.reverse()
  return path


def printGrid(grid):
  for i in range(len(grid)):
    for j in range(len(grid[i])):
      print(grid[i][j], end='')
    print()


with open("input.txt", 'r') as infile:
  data = infile.read().splitlines()

bytes = [(int(x.split(",")[0]), int(x.split(",")[1])) for x in data]

def part1(start, end, xLen, yLen, numBytes):
  grid = [['.' for _ in range(xLen)] for _ in range(yLen)]
  for b in range(numBytes):
    x, y = bytes[b]
    grid[y][x] = "#"

  smallestPath = a_star(grid, start, end)

  print(len(smallestPath[1:]), smallestPath)

def part2(start, end, xLen, yLen):
  grid = [['.' for _ in range(xLen)] for _ in range(yLen)]
  byte = (-1, -1)
  for b in bytes:
    x, y = b
    grid[y][x] = "#"
    p = a_star(grid, start, end)
    if len(p) == 0:
      byte = b
      break

  print(byte)


part1((0, 0), (70, 70), 71, 71, 1024)
part2((0, 0), (70, 70), 71, 71)
