# Stdlib
from sys import argv, setrecursionlimit

# 3rd party
import numpy as np

setrecursionlimit(100000)


def get_neighbours(
    grid: np.ndarray, route: tuple[tuple[int, int], ...]
) -> set[tuple[int, int]]:

    point = route[-1]
    visited = {_ for _ in route}

    row, col = point
    neighbours = set()
    for r, c in [(row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1)]:
        if (r, c) in visited:
            continue
        if r >= 0 and r < grid.shape[0] and c >= 0 and c < grid.shape[1]:
            if grid[r, c] == ".":
                neighbours.add((r, c))
    return neighbours


def solve(
    grid: np.ndarray,
    routes: set[tuple[tuple[int, int], ...]],
    best_route: None | tuple[tuple[int, int], ...] = None,
) -> tuple[tuple[int, int], ...]:

    new_routes = set()
    target = tuple(map(lambda _: _ - 1, grid.shape))

    best_routes = sorted(routes, key=lambda _: -_[-1][0] - _[-1][1])

    if best_route is None:
        route = best_routes[0]
    else:
        for route in best_routes:
            if len(route) < len(best_route):
                break
        else:
            return best_route

    new_routes = {_ for _ in routes if _ != route}
    neighbours = get_neighbours(grid, route)

    if len(route) % 50 == 0:
        print(route[-1])

    if target in neighbours:
        candidate = tuple(list(route) + [target])
        if best_route is None:
            best_route = candidate
            print(f"Found first best route of length : {len(best_route)}")
        elif len(candidate) < len(best_route):
            best_route = candidate
            print(f"Found new best route of length : {len(best_route)}")

    for neighbour in neighbours:
        new_route = tuple(list(route) + [neighbour])
        new_routes.add(new_route)

    return solve(grid, new_routes, best_route)


if __name__ == "__main__":
    SIZE = int(argv[1]) + 1
    FILE_PATH = argv[2]
    BYTES_FALLEN = int(argv[3])
    GRID = np.full((SIZE, SIZE), ".")

    with open(argv[2], "r") as flin:
        for idx, ln in enumerate(flin):
            if idx == BYTES_FALLEN:
                break

            col, row = map(int, ln.split(","))
            GRID[row, col] = "#"
    print(GRID)

    optimal_route = solve(GRID, {((0, 0),)})
    print(len(optimal_route) - 1)  # The -1 here is because we don't count 0,0
