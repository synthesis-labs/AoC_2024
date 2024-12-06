from functools import cmp_to_key

with open('data.txt', 'r') as f:
    data = f.readlines()

ordering_rules_input = [l.strip() for l in data if "|" in l]
updates_input = [l.strip().split(",") for l in data if "|" not in l and len(l) > 1]

graph = {}
for l in ordering_rules_input:
    k, v = l.split('|')
    # print(f"k: {k}, v: {v}")
    if k in graph:
        graph[k].append(v)
    else:
        graph[k] = [v]
    if v not in graph:
        graph[v] = []

def is_ordered(pages) -> bool:
    for i in range(len(pages) - 1):
        for j in range(i + 1, len(pages)):
            if pages[j] not in graph[pages[i]]:
                return False
    return True

acc = sum(int(u[len(u) // 2]) for u in updates_input if is_ordered(u))
print(f"Part 1: {acc}")

# Part 2
def compare(x, y):
    if x in graph[y]:
        return -1
    elif y in graph[x]:
        return 1
    else:
        return 0

def sort_pages(pages):
    return sorted(pages, key=cmp_to_key(compare))

unordered = [u for u in updates_input if not is_ordered(u)]
# print(unordered)
ordered = [sort_pages(u) for u in unordered]
# print(ordered)
middles = [u[len(u) // 2] for u in ordered]
# print(middle)
print(f"Part 2: {sum(map(int, middles))}")



