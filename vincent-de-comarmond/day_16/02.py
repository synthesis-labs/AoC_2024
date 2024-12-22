# stdlib
from dataclasses import dataclass
from pprint import pp
from sys import argv, setrecursionlimit

# 3rd party
import numpy as np

# setrecursionlimit(1000000)


@dataclass(eq=True, frozen=True)
class Point:
    row: int
    col: int
    nesw: int
    burndown: int
    energy: int
    history: frozenset[tuple[int, int]]


def load_input(txt_file: str) -> np.ndarray:
    racetrack: list[list[str]] = []
    with open(txt_file, "r") as txtin:
        racetrack = [[_ for _ in line if _ != "\n"] for line in txtin]
    return np.array(racetrack)


def get_action_space(racetrack: np.ndarray, point: Point) -> set[Point]:
    r, c = point.row, point.col
    hist, energy = point.history, point.energy
    neighbours = set()
    _shape = racetrack.shape
    for row, col, nesw in [(r - 1, c, 0), (r, c + 1, 1), (r + 1, c, 2), (r, c - 1, 3)]:
        if any((row < 0, col < 0, row >= _shape[0], col >= _shape[1])):
            continue
        if racetrack[row, col] not in (".", "E"):
            continue

        if (row, col) in point.history:
            continue

        burndown = 1 if nesw == point.nesw else 1001
        if energy >= burndown:
            neighbours.add(
                Point(
                    row,
                    col,
                    nesw,
                    burndown,
                    energy - burndown,
                    hist | frozenset({(row, col)}),
                )
            )

    return neighbours


def get_start(
    racetrack: np.ndarray, energy: int = 98484
) -> tuple[int, int, tuple[int, int]]:
    row, col = (_[0] for _ in np.where(racetrack == "S"))
    return Point(row, col, 1, 1, energy, frozenset({(row, col)}))


def solve_dfs_with_energy_limit(
    racetrack: np.ndarray, active: set[Point]
) -> list[tuple[int, int]]:

    optimal_routes = set()
    burnt = set()

    while len(active) > 0:
        point = active.pop()

        while point.energy > 0:
            burnt.add(point)
            neighbours = get_action_space(racetrack, point)
            neighbours = [pt for pt in neighbours if (pt.row, pt.col) not in burnt]

            if len(neighbours) <= 0:
                break

            point = neighbours[0]
            active += neighbours[1:]

            if racetrack[point.row, point.col] == "E":
                optimal_routes.add(point.history)
                print(f"New optimal found - routes found: {len(optimal_routes)}")

    return optimal_routes


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


####################
# NOTES/ GRAVEYARD #
####################


# def solve(
#     racetrack: np.ndarray, active: set[Point], burnt: None | set[tuple[int, int]] = None
# ) -> set[tuple[int, int]]:
#     new_active = set()
#     burnt = set() if burnt is None else burnt

#     # Reduce recursion and recursive stack frame size by burning as much time
#     # as possible for each iteration.
#     # Python does not optimize tail-call recursion, so even something uninvolved
#     # can blow up. Then again, I'm probably not organizing my stack correctly
#     timelapse = min((_.burndown for _ in active))
#     burnme = set()
#     best_tiles = set()

#     for old_pt in active:
#         r, c, n = old_pt.row, old_pt.col, old_pt.nesw
#         hist = old_pt.history
#         point = Point(r, c, n, old_pt.burndown - timelapse, old_pt.score, hist)
#         if point.burndown == 0:
#             location = (point.row, point.col)
#             burnme.add(location)
#             neighbours = get_action_space(racetrack, point)
#             for neighbour in neighbours:
#                 if racetrack[neighbour.row, neighbour.col] == "E":
#                     best_tiles |= neighbour.history

#                 if (neighbour.row, neighbour.col) not in burnt:
#                     new_active.add(neighbour)
#         else:
#             new_active.add(point)

#     if len(best_tiles) > 0:
#         return best_tiles

#     # All winning paths should resolve at the same time
#     # Okay ... the above was the initial theory ... here's the problem (and hence small modifications):
#     #    1) Given the different burn rates around corners, different routes may reach the same tile at different times
#     #    2) It does NOT follow that a route that reaches a tile later is worse, because it may be better orientated,
#     #       and being better orientated means that it may have a better burn-rate at some future point
#     #    3) This means that 2 equally optimal paths may have the same total burn time, but one out burns another in a region
#     #       and acts as a firebreak for an equally optimal path
#     # burnt |= burnme
#     # return solve(racetrack, new_active, burnt)
#     return solve(racetrack, new_active, burnme)
