import re

with open('data.txt', 'r') as f:
    data = f.readlines()

data = [list(x.strip()) for x in data]
# print(data)

def start_position(data):
    # find the coordinate of ^
    pos = [0, 0]
    for i in range(len(data)):
        if '^' in data[i]:
            pos[0] = i
            pos[1] = data[i].index('^')
            break
    return pos


def run(g, grid):
    d = 'N'
    visited = [[0] * len(grid[0]) for _ in range(len(grid))]
    # print(visited)
    while True:
        # print(g)
        visited[g[0]][g[1]] += 1
        if visited[g[0]][g[1]] > 10:   # Is 10 enough? This is hacky but works to detect a loop.
            return 0
        # `check if next step is off the grid
        if d == 'N' and g[0] - 1 < 0:
            break
        elif d == 'E' and g[1] + 1 >= len(grid[0]):
            break
        elif d == 'S' and g[0] + 1 >= len(grid):
            break
        elif d == 'W' and g[1] - 1 < 0:
            break

        if d == 'N' and grid[g[0] - 1][g[1]] != '#':
            g[0] -= 1
        elif d == 'E' and grid[g[0]][g[1] + 1] != '#':
            g[1] += 1
        elif d == 'S' and grid[g[0] + 1][g[1]] != '#':
            g[0] += 1
        elif d == 'W' and grid[g[0]][g[1] - 1] != '#':
            g[1] -= 1
        else:
            # we have to turn
            if d == 'N':
                d = 'E'
            elif d == 'E':
                d = 'S'
            elif d == 'S':
                d = 'W'
            elif d == 'W':
                d = 'N'

    return sum([1 for i in visited for j in i if j > 0])

guard = start_position(data)
print(f"Part 1: {run(guard, data)}")

# Lets brute force part 2. hahaha
obstacles = [[False] * len(data[0]) for _ in range(len(data))]
for i in range(len(data)):
    for j in range(len(data[0])):
        if data[i][j] == '#' or data[i][j] == '^':
            continue
        # create a copy of data
        new_data = [x.copy() for x in data]
        new_data[i][j] = '#'
        guard = start_position(new_data)
        if run(guard, new_data) == 0:
            obstacles[i][j] = True
            # print(f"Found a valid obstacle at {i}, {j}")

print(f"Part 2: {sum([sum([1 for j in i if j]) for i in obstacles])}")
