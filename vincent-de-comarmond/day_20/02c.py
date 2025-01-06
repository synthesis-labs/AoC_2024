from collections import Counter
from copy import copy
from functools import lru_cache
from pprint import pp

Point = tuple[int, int]
Track = dict[Point, str]


def load_input(txt_file: str) -> tuple[Point, Point, Track, Track]:
    track = dict()
    walls = dict()
    start, end = None, None

    with open(txt_file, "r") as txtin:
        for row, line in enumerate(txtin):
            cleaned = line.strip()
            for col, char in enumerate(cleaned):
                if char in ("S", "E", "."):
                    track[row, col] = char
                else:
                    walls[row, col] = char

                end = (row, col) if char == "E" else end
                start = (row, col) if char == "S" else start

    return start, end, track, walls


@lru_cache(maxsize=None)
def get_neighbours(track_keys: frozenset[Point], point: Point) -> set[Point]:
    y, x = point
    test = {(y + r, x + c) for r, c in [(-1, 0), (0, 1), (1, 0), (0, -1)]}
    return test.intersection(track_keys)


def solve(racetrack: Track, start: Point, end: Point) -> int:
    active = {start: 0}
    burnt = set()

    while len(active) > 0:
        point, time = min(active.items(), key=lambda _: _[1])
        _ = active.pop(point)  # Remove the minimum point from the active list

        burnt.add(point)
        neighbours = get_neighbours(frozenset(racetrack), point) - burnt
        if end in neighbours:
            return time + 1

        active |= {pt: time + 1 for pt in neighbours}


@lru_cache(maxsize=None)
def make_jump_gates(
    track: frozenset[Point], walls: frozenset[Point], cheat_length: int
) -> dict[frozenset[Point], int]:
    wall_copy = set(walls)
    jump_gates = dict()

    counter = 0
    computations = len(track) * len(track) - 1

    for start in track:
        for end in track:
            if counter % 500 == 0:
                print(f"Computing jump gate {counter} of a potential {computations}")
            counter += 1

            hamming = abs(end[0] - start[0]) + abs(end[1] - start[1])
            if hamming < 2 or cheat_length < hamming:
                continue
            if start == end:
                continue

            gates = frozenset((start, end))
            wall_copy.add(start)
            wall_copy.add(end)
            time = solve(wall_copy, start, end)
            if time is not None and time <= cheat_length:
                jump_gates[frozenset((start, end))] = time
            wall_copy.discard(start)
            wall_copy.discard(end)

    return jump_gates


def solve_with_cheat(
    racetrack: Track,
    walls: Track,
    start: Point,
    end: Point,
    truncate_time: int,
    jumpgates: dict[frozenset[Point], int],
):

    done = set()
    used_cheat = False
    prev_best = -float("inf")
    routes = {((start, used_cheat, 0),)}  # Read point, used_cheat, track_time

    while min(map(lambda _: _[-1][-1], routes)) < truncate_time:

        current_best = min(map(lambda _: _[-1][-1], routes))
        if current_best > prev_best:
            print(f"Minimum time of {current_best}. Max allowed is {truncate_time}")
            prev_best = current_best

        new_routes = set()
        for route in routes:
            point, used_cheat, time = route[-1]

            if time > truncate_time:
                continue

            if point == end:
                done.add(route)
                continue

            visited = {_[0] for _ in route}
            neighbours = get_neighbours(frozenset(racetrack), point) - visited

            for ngh in neighbours:
                new_routes.add(route + ((ngh, used_cheat, time + 1),))

            if not used_cheat:
                for k, v in jump_gates.items():
                    if point in k:
                        _exit = [_ for _ in k if _ != point][0]
                        new_routes.add(route + ((_exit, True, time + v),))

        routes = new_routes
    return done


def print_track(
    racetrack: Track, walls: Track, routes: list[dict[Point, str]] = list()
) -> None:
    unified = {**racetrack, **walls}

    for route in routes:
        for point, display in route.items():
            unified[point] = display

    prev_row = 0
    for (row, col), v in sorted(unified.items()):
        if row != prev_row:
            prev_row = row
            print("")
        print(v, end="")
    print("")


start, end, track, walls = load_input("./sample.txt")
best_time = solve(track, start, end)
jump_gates = make_jump_gates(frozenset(track), frozenset(walls), 20)

cheat_solns = list(
    solve_with_cheat(track, walls, start, end, best_time - 50, jump_gates)
)
counts = Counter([_[-1][-1] for _ in cheat_solns])
savings = dict(sorted([(best_time - k, v) for k, v in counts.items()]))
pp(savings)

if True:
    for saved in (72,):
        print(f"#####\nSaved: {saved}\n#####")
        for cheat in sorted([_ for _ in cheat_solns if _[-1][-1] == best_time - saved]):
            routes = [{_[0]: "x" for i, _ in enumerate(cheat[:-1]) if i > 0}]
            print_track(track, walls, routes)
