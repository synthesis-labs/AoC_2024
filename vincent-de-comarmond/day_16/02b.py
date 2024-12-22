# stdlib
from dataclasses import dataclass
from pprint import pp
from sys import argv, setrecursionlimit

# 3rd party
import numpy as np

Point = tuple[int, int]


def load_input(txt_file: str) -> np.ndarray:
    racetrack: list[list[str]] = []
    with open(txt_file, "r") as txtin:
        racetrack = [[_ for _ in line if _ != "\n"] for line in txtin]
    return np.array(racetrack)


def make_graph(racetrack: np.ndarray) -> dict[Point, list[Point]]:
    track_graph = dict()
    for r, row in enumerate(racetrack):
        for c, char in enumerate(row):
            if char in (".", "S"):
                track_graph[r, c] = set()
            else:
                continue

            for _r, _c in [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]:
                _char = racetrack[_r, _c]
                if _char in (".", "S", "E"):
                    track_graph[r, c].add((_r, _c))

    return track_graph


def get_start(racetrack: np.ndarray) -> tuple[int, int]:
    row, col = tuple((int(_[0]) for _ in np.where(racetrack == "S")))
    return row, col


def get_end(racetrack: np.ndarray) -> tuple[int, int]:
    row, col = tuple((int(_[0]) for _ in np.where(racetrack == "E")))
    return row, col


def make_traversal(
    graph: dict[Point, list[Point]], start: Point, end: Point
) -> dict[Point, int]:

    complete_tracks = set()
    
    active: list[Point] = [start]
    while len(active) > 0:
        point = active.pop()
        traversal: dict[Point, int] = dict()

        while point != end:
            traversal[point] = 1
            neighbours = graph[point] - set(traversal)
            if len(neighbours) == 0:
                break
            if len(neighbours) >= 1:
                point = neighbours.pop()
                active += list(neighbours)
                if point == end:
                    traversal[point] += 1
                    print(traversal)
                    complete_tracks.add(tuple(traversal.items()))
    print(complete_tracks)
    return complete_tracks


def print_ndarray(input_array: np.ndarray) -> None:
    print("\n".join(["".join([str(_) for _ in line]) for line in input_array]))


if __name__ == "__main__":
    # 98484 is the right answer for part 1
    racetrack = load_input(argv[1])
    start = get_start(racetrack, int(argv[2]))
    best_routes = solve_dfs_with_energy_limit(racetrack, [start])
    best_tiles = {tile for route in best_routes for tile in route}

    for y, x in best_tiles:
        racetrack[y, x] = "O"

    print_ndarray(racetrack)
    print(len(best_tiles))
