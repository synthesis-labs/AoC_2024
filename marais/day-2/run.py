# read data.txt

with open('data.txt', 'r') as f:
    data = f.readlines()

# split data into integers
data = [list(map(int,x.split())) for x in data]

def is_valid(l) -> bool:
    diffs = [l[i] - l[i - 1] for i in range(1, len(l))]
    if all([0 < d < 4 for d in diffs]):
        return True
    elif all([-4 < d < 0 for d in diffs]):
        return True
    else:
        return False

valid_count = sum([is_valid(l) for l in data])
print(f"Part 1: {valid_count}")

# Part 2

def is_valid_two(l) -> bool:
    if is_valid(l):
        return True

    new_l = [l[:i] + l[i+1:] for i in range(0, len(l))]
    if any([is_valid(d) for d in new_l]):
        return True

    return False

valid_count_two = sum([is_valid_two(l) for l in data])
print(f"Part 2: {valid_count_two}")

