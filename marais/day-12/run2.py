from PIL import Image, ImageDraw

with open('test.txt', 'r') as f:
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

# Each character represents a cell in a grid. Each cell is a polygon.

class Poly:
    def __init__(self, vertices: [], char, color=None):
        self.vertices = vertices
        self.char = char
        self.color = color

    def __str__(self):
        return f'{self.char}, color: {self.color}'

polygons = []

for y, line in enumerate(data):
    for x, char in enumerate(line.strip()):
        p = Poly([(x, y), (x+1, y), (x+1, y+1), (x, y+1)], char, ((y + 1) * 10, (x + 1) * 10, 50))
        polygons.append(p)

def scale_polygon(polygon, scale=100):
    return Poly([(v[0] * scale, v[1] * scale) for v in polygon.vertices], polygon.char, polygon.color)

def print_map(polygons, scale=100):
    max_x = max([max([v[0] for v in p.vertices]) for p in polygons]) * scale
    max_y = max([max([v[1] for v in p.vertices]) for p in polygons]) * scale
    img = Image.new("RGB", (max_x, max_y), "#f9f9f9")
    img1 = ImageDraw.Draw(img)
    for p in polygons:
        # scale the polygon
        p = scale_polygon(p, scale)
        img1.polygon(p.vertices, p.color, outline=(0, 0, 0))
        img1.text(p.vertices[0], p.char, fill=(255, 255, 255))

    img.show()

def flood_fill(poly, polygons):
    # Find all adjacent polygons to poly with the same char and merge them
    for p in polygons:
        if p.char == poly.char:
            for v in p.vertices:
                if v in poly.vertices:
                    # merge the two polygons
                    poly.vertices.extend(p.vertices)
                    polygons.remove(p)
                    break
    # remove duplicates
    poly.vertices = list(set(poly.vertices))
    return poly

def merge_polygons(polygons):
    for p in polygons:
        flood_fill(p, polygons)

    return polygons

polygons = merge_polygons(polygons)

print_map(polygons, 20)

