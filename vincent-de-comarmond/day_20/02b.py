# stdlib
from collections import Counter, defaultdict
from dataclasses import dataclass
from pprint import pp
from sys import argv


@dataclass()
class PuzzleData:
    end: int
    height: int
    neighbours: dict[int, list[int]]
    start: int
    track: dict[int, str]
    width: int


def load_input(txt_file) -> PuzzleData:
    track = dict()
    start, end = None, None

    with open(txt_file, "r") as txtin:
        for height, line in enumerate(txtin):
            cleaned = line.strip()
            for char in cleaned:
                end = len(track) if char == "E" else end
                start = len(track) if char == "S" else start
                track[len(track)] = char
    width = len(cleaned)

    neighbours = dict()
    for el in track:
        l_edge = el % width == 0
        r_edge = (el + 1) % width == 0

        left = None if l_edge else el - 1
        right = None if r_edge else el + 1
        down = el + width if el < len(track) - width else None
        up = el - width if el >= width else None

        neighbours[el] = set(filter(None, [up, right, down, left]))

    return PuzzleData(end, height, neighbours, start, track, width)


def print_track(track: dict[int, str], width: int) -> None:
    for k, v in sorted(track.items()):
        if k % width == 0 and k != 0:
            print("")
        print(v, end="")


def determine_jumpholes(
    track: dict[int, str],
    neighbours: dict[int, list[int]],
    width: int,
    max_cheat_length: int,
) -> dict[frozenset[int], int]:

    walls = {k: v for k, v in track.items() if v == "#"}
    starts_ends = set()
    for wall in walls:
        starts_ends |= {_ for _ in neighbours[wall] if track[_] != "#"}

    potential_pairs: set[frozenset[int]] = set()
    for start in starts_ends:
        for end in starts_ends:
            if start == end:
                continue

            sy, sx = start // width, start % width
            ey, ex = end // width, end % width
            hamming = abs(ey - sy) + abs(ex - sx)
            # Can't be next to each other
            if hamming <= max_cheat_length + 1 and 1 < hamming:
                potential_pairs.add(frozenset((start, end)))
    real_pairs = dict()
    print(f"Number potential pairs: {len(potential_pairs)}")
    for idx, pair in enumerate(potential_pairs):
        if idx % 100 == 0:
            print(f"Filtered {idx} of {len(potential_pairs)} potential_pairs.")
        start, end = pair
        cheat_route = solve_inverse_optimally(track, neighbours, start, end)

        # -1 Here because the start point shouldn't be included
        if cheat_route is not None and 0 < len(cheat_route):
            if len(cheat_route) - 1 <= max_cheat_length:
                real_pairs[pair] = len(cheat_route) - 1
    return real_pairs


def solve_inverse_optimally(
    track: dict[int, str], neighbours: dict[int, list[int]], start: int, end: int
) -> tuple[int, ...]:

    active: set[tuple[int, ...]] = {(start,)}
    completed: set[tuple[int, ...]] = set()

    while len(active) > 0:
        hist = active.pop()

        if hist[-1] == end:
            completed.add(hist)
            return hist

        # Cannot go back on itself
        nghs = neighbours[hist[-1]] - set(hist)
        nghs = {_ for _ in nghs if _ in (start, end) or track[_] == "#"}
        active |= {hist + (_,) for _ in nghs}


def solve_optimally(
    track: dict[int, str],
    neighbours: dict[int, list[int]],
    start: int,
    end: int,
    jumpholes: dict[frozenset[int], int],
    truncate: int,
) -> tuple[int, ...]:

    active: set[tuple[int, ...]] = {((start, 1, False),)}
    completed: set[tuple[int, ...]] = set()

    while len(active) > 0:
        hist = active.pop()

        pos, time, ghosted = hist[-1]

        # if ghosted:
        #     pp(hist)

        if time > truncate:
            continue

        if pos == end:
            completed.add(hist)
            continue

        # Cannot go back on itself
        nghs = neighbours[pos] - {_[0] for _ in hist}

        additions: set[int] = set()
        additions = {(_, time + 1, ghosted) for _ in nghs if track[_] != "#"}

        if not ghosted:
            for n in nghs:
                for k, v in jumpholes.items():
                    if n not in k:
                        continue

                    other = [_ for _ in k if _ != n][0]
                    additions.add((other, time + v + 1, True))

        active |= {hist + (_,) for _ in additions}

    return completed


if __name__ == "__main__":
    FILE_PATH = argv[1]
    CHEAT_LENGTH = int(argv[2])
    GOOD_CHEAT = int(argv[3])

    puzzle_data = load_input(FILE_PATH)
    # We need the base time to evaluate how good the cheats are
    base_time = min(
        map(
            len,
            solve_optimally(
                puzzle_data.track,
                puzzle_data.neighbours,
                puzzle_data.start,
                puzzle_data.end,
                dict(),
                1000000000,
            ),
        )
    )

    jumpholes = determine_jumpholes(
        puzzle_data.track, puzzle_data.neighbours, puzzle_data.width, CHEAT_LENGTH
    )
    pp(jumpholes)

    cheat_routes = solve_optimally(
        puzzle_data.track,
        puzzle_data.neighbours,
        puzzle_data.start,
        puzzle_data.end,
        jumpholes,
        base_time - GOOD_CHEAT,
    )

    path_counts = Counter(map(lambda _: base_time - _[-1][1], cheat_routes))
    improvements = {k: v for k, v in path_counts.items() if k != 0}
    pp(dict(sorted(improvements.items())))
