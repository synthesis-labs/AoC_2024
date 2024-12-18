# stdlib
from pprint import pprint
from sys import argv

# 3rd party
import numpy as np


def load_input(txt_file: str) -> np.ndarray:
    racetrack: list[list[str]] = []
    with open(txt_file, "r") as txtin:
        racetrack = [[_ for _ in line if _ != "\n"] for line in txtin]
    return np.array(racetrack)


def get_start(racetrack: np.ndarray) -> tuple[int, int, tuple[int, int]]:
    row, col = (_[0] for _ in np.where(racetrack == "S"))
    return (row, col, (1, 0), 0)


def get_action_space(racetrack: np.ndarray, route: tuple[int, int, tuple[int], int]):

    action_space = set()
    active_point = route[-1]
    visited = {(_[0], _[1]) for _ in route}

    _max = racetrack.shape

    for _dir in ((-1, 0), (0, -1), (1, 0), (0, 1)):
        _loc = tuple(np.add(active_point[:2], _dir))
        if _loc[0] < 0 or _loc[1] < 0 or _loc[0] >= _max[0] or _loc[1] >= _max[1]:
            continue

        # Do not revisit the same point
        if _loc in visited:
            continue

        if racetrack[_loc[0], _loc[1]] in (".", "E"):
            new_score = active_point[-1]
            new_score += 1 if tuple(_dir) == active_point[2] else 1001
            action_space.add((*_loc, _dir, new_score))

    return action_space


def find_optimal_route(input_fp: str) -> tuple[tuple[int, int, tuple[int, int], int]]:
    track = load_input(input_fp)
    active_routes = {(get_start(track),)}
    completed_routes = set()

    best_score = float("inf")
    best_routes_found = 0

    while len(active_routes) > 0:
        # DO DFS with Pruning
        route = active_routes.pop()

        while route is not None:
            break_dfs = True
            action_space = get_action_space(track, route)

            for idx, action in enumerate(action_space):
                if action[-1] >= best_score:
                    continue

                new_route = route + (action,)
                if track[action[0], action[1]] != "E":
                    if idx == 0:
                        route = new_route
                        break_dfs = False
                    else:
                        active_routes.add(new_route)
                else:
                    if action[-1] < best_score:
                        completed_routes.add(new_route)
                        best_score = action[-1]
                        best_routes_found += 1
                        print(f"Found new best route with score: {best_score}")
                        print(f"Best routes found: {best_routes_found}")

            if break_dfs:
                break
    print(len(completed_routes))
    return min(completed_routes, key=lambda _: _[-1][-1])


if __name__ == "__main__":
    best_route = find_optimal_route(argv[1])
    top_score = best_route[-1][-1]
    print(top_score)

    # 186576 is too high
