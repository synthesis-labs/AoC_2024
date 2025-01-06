from collections import Counter, defaultdict
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


def get_best(active_routes: set[Point, int]) -> int:
    return min(map(lambda _: _[-1][-1], active_routes))


def solve_time_restricted(
    racetrack: Track,
    start: Point,
    end: Point,
    time_lim: float | int = float("inf"),
    jump_gates: dict[frozenset[Point], set[int]] = dict(),
):
    done = set()
    routes = {((start, False, 0),)}  # Read point, used_cheat, track_time

    while get_best(routes) <= time_lim:
        new_routes = set()
        for route in routes:
            point, used_cheat, time = route[-1]

            if time > time_lim:
                continue

            if point == end:
                done.add(route)
                if time_lim == float("inf"):
                    return done
                continue

            visited = {_[0] for _ in route}
            neighbours = get_neighbours(frozenset(racetrack), point) - visited
            for ngh in neighbours:
                new_routes.add(route + ((ngh, used_cheat, time + 1),))

            if not used_cheat:
                for k, v in jump_gates.items():
                    if point in k:
                        _exit = [_ for _ in k if _ != point][0]
                        for t in v:
                            new_routes.add(route + ((_exit, True, time + t),))

        routes = new_routes
    return done


@lru_cache(maxsize=None)
def make_jump_gates(
    track: frozenset[Point], walls: frozenset[Point], cheat_length: int
) -> dict[frozenset[Point], set[int]]:
    wall_copy = set(walls)
    jump_gates = defaultdict(set)

    counter = 0
    computations = len(track) * len(track) - 1

    for start in track:
        for end in track:
            if counter % 10 == 0:
                print(f"Computing jump gate {counter} of a potential {computations}")
            counter += 1

            if frozenset((start, end)) in jump_gates:
                continue

            hamming = abs(end[0] - start[0]) + abs(end[1] - start[1])
            if hamming < 2 or cheat_length < hamming:
                continue
            if start == end:
                continue

            gates = {start, end}
            wall_copy |= gates
            routes = solve_time_restricted(wall_copy, start, end, cheat_length)
            for route in routes:
                jump_gates[frozenset((start, end))].add(route[-1][-1])
            wall_copy = wall_copy - gates

    return jump_gates


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
best_route = solve_time_restricted(track, start, end)  # One of potentially many
best_time = best_route.pop()[-1][-1]

jump_gates = make_jump_gates(frozenset(track), frozenset(walls), 20)
# Make it a list here just so we can easily index for debugging
cheat_solns = solve_time_restricted(
    track, start, end, time_lim=best_time - 50, jump_gates=jump_gates
)
cheat_solns = list(cheat_solns)

cheat_savings = dict(
    sorted(Counter([best_time - _[-1][-1] for _ in cheat_solns]).items())
)
pp(cheat_savings)

# cheat_solns = list(solve_with_cheat(track, walls, start, end, 20, best_time - 50))
# counts = Counter([_[-1][-1] for _ in cheat_solns])
# savings = dict(sorted([(best_time - k, v) for k, v in counts.items()]))
# pp(savings)

# for saved in (72,):
#     print(f"#####\nSaved: {saved}\n#####")
#     for cheat in sorted([_ for _ in cheat_solns if _[-1][-1] == best_time - saved]):
#         routes = [{_[0]: "x" for i, _ in enumerate(cheat[:-1]) if i > 0}]
#         print_track(track, walls, routes)
