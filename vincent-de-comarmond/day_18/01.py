# Stdlib
from sys import argv

# 3rd party
import numpy as np


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
    grid: np.ndarray, routes: set[tuple[tuple[int, int], ...]]
) -> tuple[tuple[int, int], ...]:

    new_routes = set()
    target = tuple(map(lambda _: _ - 1, grid.shape))
    # print(target)

    print(max(routes, key=lambda _: sum(_[-1]))[-1])

    for route in routes:
        neighbours = get_neighbours(grid, route)
        # print(neighbours)
        if target in neighbours:
            return tuple(list(route) + [target])

        for neighbour in neighbours:
            new_route = tuple(list(route) + [neighbour])
            if len(new_route) > 50:
                new_route = new_route[-50:]
            new_routes.add(new_route)

    return solve(grid, new_routes)


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
