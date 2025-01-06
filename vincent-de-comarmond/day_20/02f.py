from collections import Counter, defaultdict
from copy import copy
from functools import lru_cache, partial
from pprint import pp
from sys import argv

Point = tuple[int, int]
Track = frozenset[Point]


def load_input(txt_file: str) -> tuple[Point, Point, Track, Track]:
    track = set()
    walls = set()
    start, end = None, None

    with open(txt_file, "r") as txtin:
        for row, line in enumerate(txtin):
            cleaned = line.strip()
            for col, char in enumerate(cleaned):
                if char in ("S", "E", "."):
                    track.add((row, col))
                else:
                    walls.add((row, col))

                end = (row, col) if char == "E" else end
                start = (row, col) if char == "S" else start

    return start, end, frozenset(track), frozenset(walls)


@lru_cache(maxsize=None)
def get_neighbours(
    track: Track, point: Point, min_radius: int = 1, max_radius: int = 1
) -> set[tuple[Point, int]]:
    y, x = point
    neighbours = set()
    for dy in range(-max_radius, max_radius + 1):
        for dx in range(-max_radius, max_radius + 1):
            hamming = abs(dx) + abs(dy)
            if hamming < min_radius or max_radius < hamming:
                continue

            ngh = y + dy, x + dx
            if ngh in track:
                neighbours.add((ngh, hamming))

    return neighbours


def get_best(active_routes: set[Point, int]) -> int:
    return min(map(lambda _: _[-1][-1], active_routes))


def solve_time_restricted(
    track: Track,
    start: Point,
    end: Point,
    time_lim: float | int = float("inf"),
    cheat_length: int = 0,
) -> dict[int, int]:

    best = 0
    results = defaultdict(int)
    routes = [((start, False, best),)]  # Read point, used_cheat, track_time

    _min, _max = 2, cheat_length
    nearest_neighbours = partial(get_neighbours, track)
    cheat_neighbours = partial(get_neighbours, track, min_radius=_min, max_radius=_max)

    while best <= time_lim and 0 < len(routes):

        # Do depth first rather than bredth first
        route = routes.pop()
        point, used_cheat, time = route[-1]
        if time_lim < time:
            continue

        if point == end:
            if time not in results:
                print(f"Found new solution of time: {time}")
                print(f"Number of routes: {len(routes)}")

            results[time] += 1
            if time_lim == float("inf"):
                return results
            continue

        visited = {_[0] for _ in route}
        neighbours = {_ for _ in nearest_neighbours(point) if _[0] not in visited}

        for ngh in neighbours:
            routes.append(route + ((ngh[0], used_cheat, time + 1),))

        if used_cheat or cheat_length <= 1:
            continue

        far_neighbours = {_ for _ in cheat_neighbours(point) if _[0] not in visited}
        for ngh in far_neighbours:
            routes.append(route + ((ngh[0], True, time + ngh[1]),))

        best = get_best(routes)

    return dict(results)


if __name__ == "__main__":
    FILE_PATH = argv[1]
    CHEAT_LENGTH = int(argv[2])
    MIN_TIME_SAVING = int(argv[3])

    start, end, track, walls = load_input(FILE_PATH)
    # One of potentially many
    best_counts_times = solve_time_restricted(track, start, end)
    best_time, best_count = best_counts_times.popitem()
    print(f"Best time: {best_time}")

    cheat_solns = solve_time_restricted(
        track,
        start,
        end,
        time_lim=best_time - MIN_TIME_SAVING,
        cheat_length=CHEAT_LENGTH,
    )
    # alpha = dict(sorted(cheat_solns.items()))
    # pp({best_time - k: v for k, v in alpha.items()})
    print(f"Number of cheats which save you {MIN_TIME_SAVING} or more seconds:")
    print(sum(cheat_solns.values()))
