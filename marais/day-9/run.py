import itertools

with open('data.txt', 'r') as f:
    data = f.readlines()

data = list(map(int, data[0].strip()))


def create_block_map(data):
    data = data.copy()
    fid = 0
    block_map = []
    # treat data as a queue and pop the first 2 elements
    while len(data) > 0:
        f_length = data.pop(0)
        empty = data.pop(0) if len(data) > 0 else 0
        # print(f_length, empty)
        for _ in range(f_length):
            block_map.append(fid)
        for _ in range(empty):
            block_map.append(-1)
        fid += 1
    return block_map


def defrag1(block_map):
    end_p = len(block_map) - 1
    p = 0
    while p < end_p:
        if block_map[p] == -1:
            block_map[p] = block_map[end_p]
            block_map[end_p] = -1
            while block_map[end_p] == -1:
                end_p -= 1
        p += 1
    return block_map


def defrag2(block_map):
    end_p = len(block_map) - 1
    while end_p > 0:
        # print(f"end_p: {end_p}")
        if block_map[end_p] != -1:
            # this is the end of the file
            fid = block_map[end_p]
            start_p = end_p
            while start_p > 1 and block_map[start_p - 1] == fid:
                start_p -= 1
            # found the start of the file
            length = end_p - start_p + 1
            # print(f"fid: {fid}, start_p: {start_p}, end_p: {end_p}, length: {length}")
            # scan for empty blocks of length from start of block_map
            p = 0
            found = False
            while p < len(block_map) and p < start_p:
                if block_map[p] == -1:
                    # check if there are enough empty blocks
                    if all([block_map[p + i] == -1 for i in range(length)]):
                        # print(f"found {length} empty blocks at {p}")
                        # print(block_map)
                        found = True
                        # fill in the empty blocks
                        for i in range(length):
                            block_map[p + i] = fid
                            block_map[start_p + i] = -1
                        # print(block_map)
                        # print()
                        end_p = start_p + 1
                        break
                p += 1
            if not found:
                end_p = start_p
        end_p -= 1
    return block_map


def checksum(block_map):
    return sum([i * block_map[i] for i in range(len(block_map)) if block_map[i] != -1])

block_map1 = defrag1(create_block_map(data))
block_map2 = defrag2(create_block_map(data))
print(f"Part 1: {checksum(block_map1)}")
print(f"Part 2: {checksum(block_map2)}")