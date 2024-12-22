# stdlib
from collections import defaultdict
from dataclasses import dataclass
from pprint import pp
from sys import argv
from typing import Generator

# 3rd party
import numpy as np

Point = tuple[int, int]


def load_input(txt_file: str) -> np.ndarray:
    racetrack: list[list[str]] = []
    with open(txt_file, "r") as txtin:
        racetrack = [[_ for _ in line if _ != "\n"] for line in txtin]
    return np.array(racetrack)


def get_start(racetrack: np.ndarray) -> Point:
    row, col = map(int, (_[0] for _ in np.where(racetrack == "S")))
    return row, col


def get_end(racetrack: np.ndarray) -> Point:
    row, col = map(int, (_[0] for _ in np.where(racetrack == "E")))
    return row, col


def get_all_neighbours(racetrack: np.ndarray, point: Point) -> set[tuple[Point]]:
    r, c = point
    neighbours = set()
    _shape = racetrack.shape

    # Normal Racing
    for row, col in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
        if any((row < 0, col < 0, row >= _shape[0], col >= _shape[1])):
            continue
        neighbours.add((row, col))
    return neighbours


def get_neighbours(racetrack: np.ndarray, point: Point) -> set[tuple[Point]]:
    r, c = point
    neighbours = set()
    _shape = racetrack.shape

    # Normal Racing
    for row, col in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
        if any((row < 0, col < 0, row >= _shape[0], col >= _shape[1])):
            continue
        if racetrack[row, col] not in (".", "E", "C"):
            continue
        neighbours.add((row, col))
    return neighbours


def get_inverse_neighbours(
    racetrack: np.ndarray, point: Point, start: Point, end: Point
) -> set[tuple[Point]]:
    r, c = point
    neighbours = set()
    _shape = racetrack.shape

    # Normal Racing
    for row, col in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
        if any((row < 0, col < 0, row >= _shape[0], col >= _shape[1])):
            continue
        if racetrack[row, col] == "#" or (row, col) == end or (row, col) == start:
            neighbours.add((row, col))
    return neighbours


def solve(racetrack: np.ndarray, start: Point, end: Point) -> int:
    active = {start: -1}
    burnt = set()
    time = -1  # To acount for intiialization step

    while len(active) > 0:
        point, time = min(active.items(), key=lambda _: _[1])
        _ = active.pop(point)  # Remove the minimum point from the active list

        if point == end:
            return time + 1

        burnt.add(point)
        neighbours = get_neighbours(racetrack, point) - burnt
        active |= {pt: time + 1 for pt in neighbours}


def solve_simple_inverse(racetrack: np.ndarray, start: Point, end: Point) -> int:
    active = {start: -1}
    burnt = set()
    time = -1  # To acount for intiialization step

    while len(active) > 0:
        point, time = min(active.items(), key=lambda _: _[1])
        _ = active.pop(point)  # Remove the minimum point from the active list

        if point == end:
            return time  # Don't increment time here

        burnt.add(point)
        neighbours = get_inverse_neighbours(racetrack, point, start, end) - burnt
        active |= {pt: time + 1 for pt in neighbours}


def solve_complex(
    racetrack: np.ndarray,
    jumphole_pair: frozenset[tuple[int, int]],
    jumphole_distance: int,
    start: Point,
    end: Point,
) -> int:

    has_cheated = False
    time = -1  # To acount for intiialization step
    active = {start: (-1, has_cheated)}
    burnt = set()

    while len(active) > 0:
        point, (time, has_cheated) = min(active.items(), key=lambda _: _[1])
        _ = active.pop(point)  # Remove the minimum point from the active list

        if point == end:
            return time + 1

        burnt.add(point)
        neighbours = get_neighbours(racetrack, point)
        additions = {pt: (time + 1, has_cheated) for pt in neighbours}

        if not has_cheated and point in jumphole_pair:
            other_side = [_ for _ in jumphole_pair if _ != point][0]
            if other_side not in burnt:
                additions[other_side] = (time + jump_dist + 1, True)

        active |= additions


def determine_cheat_jumpholes(
    racetrack: np.ndarray, max_cheat_length: int
) -> dict[frozenset[tuple[int, int]], int]:

    my, mx = racetrack.shape
    walls = list(zip(*map(np.ndarray.tolist, np.where(racetrack == "#"))))

    # Exclude all external walls
    # walls = list(
    #     filter(lambda _: all((_[0] > 0, _[0] < my - 1, _[1] > 0, _[1] < mx - 1)), walls)
    # )

    # Get all potential starting and ending points ... this means all walls next to the track
    start_ends = set()
    for wall in walls:
        start_ends |= set(
            filter(
                lambda _: racetrack[_] in ("S", "E", "."),
                get_all_neighbours(racetrack, wall),
            )
        )

    # Get all potential start_end pairs ... note that direction is not important for a pair
    potential_pairs: set[frozenset[tuple[int, int]]] = set()
    for start in start_ends:
        for end in start_ends:
            if start == end:
                continue

            # +1 here as we're tunneling to start and end points, but these are both not walls
            if abs(end[0] - start[0]) + abs(end[1] - start[1]) <= max_cheat_length + 1:
                potential_pairs.add(frozenset({start, end}))

    real_pairs = dict()
    print(f"Number potential pairs: {len(potential_pairs)}")
    for idx, pair in enumerate(potential_pairs):
        if idx % 100 == 0:
            print(f"Filtered {idx} of {len(potential_pairs)} potential_pairs.")

        start, end = pair
        cheat_length = solve_simple_inverse(racetrack, start, end)
        if cheat_length is not None and cheat_length <= max_cheat_length:
            real_pairs[pair] = cheat_length
    # pp(dict(sorted(real_pairs.items(), key=lambda _: _[1])))
    return real_pairs


def print_ndarray(input_array: np.ndarray) -> None:
    print("\n".join(["".join([str(_) for _ in line]) for line in input_array]))


if __name__ == "__main__":

    INPUT_FP = argv[1]
    CHEAT_DURATION = int(argv[2])
    SMALLEST_GOOD_CHEAT = int(argv[3])

    racetrack = load_input(INPUT_FP)
    start = get_start(racetrack)
    end = get_end(racetrack)
    # We need the base time to evaluate how good the cheats are
    base_time = solve(racetrack, start, end)

    time_savings = defaultdict(int)
    jumpholes = determine_cheat_jumpholes(racetrack, CHEAT_DURATION)
    for idx, (jump_pair, jump_dist) in enumerate(jumpholes.items()):
        if idx % 100 == 0:
            print(f"Computing distance for cheat {idx} of {len(jumpholes)}")
        cheat_time = solve_complex(racetrack, jump_pair, jump_dist, start, end)
        time_saving = base_time - cheat_time
        time_savings[time_saving] += 1

    pp(dict(sorted(time_savings.items(), key=lambda _: _[0])))

    good_cheats = {k: v for k, v in time_savings.items() if k >= SMALLEST_GOOD_CHEAT}

    print(f"Number of cheats of at least {SMALLEST_GOOD_CHEAT} picoseconds:")
    print(sum(good_cheats.values()))
    # 1395 is the number of correct cheats for part 1
