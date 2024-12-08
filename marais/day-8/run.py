import itertools

with open('data.txt', 'r') as f:
    data = f.readlines()

data = [list(x.strip()) for x in data]
unique_chars = set(itertools.chain(*data))
print(unique_chars)
char_positions = {char: [] for char in unique_chars if char != '.'}
for y, row in enumerate(data):
    for x, char in enumerate(row):
        if char != '.':
            char_positions[char].append((x, y))

# print(char_positions)
def get_antinodes_direction(pos, dx, dy, max_x, max_y, direction, part) -> []:
    antinodes = []
    more = True
    curr_an = pos
    while more:
        more = part != 1
        new_an = (curr_an[0] - dx, curr_an[1] - dy) if direction == 1 else (curr_an[0] + dx, curr_an[1] + dy)
        if 0 <= new_an[0] < max_x and 0 <= new_an[1] < max_y:
            antinodes.append(new_an)
            curr_an = new_an
        else:
            more = False
    return antinodes

def get_antinodes(pos1, pos2, max_x, max_y, part=1) -> []:
    dx = pos2[0] - pos1[0]
    dy = pos2[1] - pos1[1]
    antinodes = [] if part == 1 else [pos1, pos2]
    antinodes.extend(get_antinodes_direction(pos1, dx, dy, max_x, max_y, 1, part))
    antinodes.extend(get_antinodes_direction(pos2, dx, dy, max_x, max_y, 2, part))
    return antinodes

# print(an_pos)
def print_grid(data, an_pos):
    for y, row in enumerate(data):
        for x, char in enumerate(row):
            if (x, y) in an_pos:
                print('#', end='')
            else:
                print(char, end='')
        print()

# Part 1
an_pos = []
for char, positions in char_positions.items():
    for pos1, pos2 in itertools.combinations(positions, 2):
        an_pos.extend(get_antinodes(pos1, pos2, len(data[0]), len(data), 1))

an_pos = set(an_pos)
# print_grid(data, an_pos)
print(f"Part 1: {len(an_pos)}")

# Part 2
an_pos = []
for char, positions in char_positions.items():
    for pos1, pos2 in itertools.combinations(positions, 2):
        an_pos.extend(get_antinodes(pos1, pos2, len(data[0]), len(data), 2))

an_pos = set(an_pos)
print(f"Part 2: {len(an_pos)}")
