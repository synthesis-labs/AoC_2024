import itertools
import networkx as nx


with open('data.txt', 'r') as f:
    data = f.readlines()

# Data:
# RRRRIICCFF
# RRRRIICCCF
# VVRRRCCFFF
# VVRCCCJFFF
# VVVVCJJCFE
# VVIVCCJJEE
# VVIIICJJEE
# MIIIIIJJEE
# MIIISIJEEE
# MMMISSJEEE

# garden map in y,x format
garden_map = [list(x.strip()) for x in data]

def find_adjacent_chars(garden_map, x, y, graph: nx.Graph, visited_points) -> None:
    # Find all coordinates for characters that are the
    # same as the one at x,y and are adjacent to it and do this recursively
    # This will return a graph of all connected characters
    c = garden_map[y][x]
    # Add all same adjacent nodes to the graph
    if y < len(garden_map) - 1 and garden_map[y + 1][x] == c:
        graph.add_edge((x, y), (x, y + 1))
    if x > 0 and garden_map[y][x - 1] == c:
        graph.add_edge((x, y), (x - 1, y))
    if y > 0 and garden_map[y - 1][x] == c:
        graph.add_edge((x, y), (x, y - 1))
    if x < len(garden_map[y]) - 1 and garden_map[y][x + 1] == c:
        graph.add_edge((x, y), (x + 1, y))

    if y < len(garden_map) - 1 and garden_map[y + 1][x] == c and (x, y + 1) not in visited_points:
        visited_points.add((x, y + 1))
        find_adjacent_chars(garden_map, x, y + 1, graph, visited_points)
    if x > 0 and garden_map[y][x - 1] == c and (x - 1, y) not in visited_points:
        visited_points.add((x - 1, y))
        find_adjacent_chars(garden_map, x - 1, y, graph, visited_points)
    if y > 0 and garden_map[y - 1][x] == c and (x, y - 1) not in visited_points:
        visited_points.add((x, y - 1))
        find_adjacent_chars(garden_map, x, y - 1, graph, visited_points)
    if x < len(garden_map[y]) - 1 and garden_map[y][x + 1] == c and (x + 1, y) not in visited_points:
        visited_points.add((x + 1, y))
        find_adjacent_chars(garden_map, x + 1, y, graph, visited_points)


def part1(garden_map):
    visited_points = set()
    graphs = []
    for y, _ in enumerate(garden_map):
        for x, _ in enumerate(garden_map[y]):
            if (x, y) not in visited_points:
                visited_points.add((x, y))
                # start a new graph
                g = nx.Graph()
                g.add_node((x, y))
                find_adjacent_chars(garden_map, x, y, g, visited_points)
                graphs.append(g)
                print(g)
    return graphs

def calc_price(graph):
    num_nodes = len(graph.nodes)
    perimeter = 0
    for node, adj_nodes in graph.adjacency():
        perimeter += 4 - len(adj_nodes)
    return perimeter * num_nodes

def part2(graphs):
    # find long edges for each graph by scanning horizontally and vertically
    for graph in graphs:
        # find min_x, min_y, max_x, max_y
        min_x = min([x for x, _ in graph.nodes])
        min_y = min([y for _, y in graph.nodes])
        max_x = max([x for x, _ in graph.nodes])
        max_y = max([y for _, y in graph.nodes])

print(f"Prat 1: {sum([calc_price(graph) for graph in part1(garden_map)])}")