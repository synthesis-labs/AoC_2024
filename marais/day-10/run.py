with open('data.txt', 'r') as f:
    data = f.readlines()

tmap = [list(map(int, x.strip())) for x in data]
# print(tmap)

trail_heads = []
for y, row in enumerate(tmap):
    for x, val in enumerate(row):
        if val == 0:
            trail_heads.append((x, y))


def build_trail(position, ends):
    x, y = position
    if tmap[y][x] == 9:
        ends.append(position)
        return
    current_height = tmap[y][x]
    if y > 0 and tmap[y - 1][x] == current_height + 1:
        build_trail((x, y - 1), ends)
    if y < len(tmap) - 1 and tmap[y + 1][x] == current_height + 1:
        build_trail((x, y + 1), ends)
    if x > 0 and tmap[y][x - 1] == current_height + 1:
        build_trail((x - 1, y), ends)
    if x < len(tmap[0]) - 1 and tmap[y][x + 1] == current_height + 1:
        build_trail((x + 1, y), ends)
    return



def score_trail_head(head) -> (int,int):
    trail_end = []
    build_trail(head, trail_end)
    return len(set(trail_end)), len(trail_end)

scores_ratings = [score_trail_head(x) for x in trail_heads]
# split the scores and ratings from each other
scores, ratings = zip(*scores_ratings)

print(f"scores: {scores}")
print(f"ratings: {ratings}")

print(f"part 1: {sum(scores)}")
print(f"part 2: {sum(ratings)}")
