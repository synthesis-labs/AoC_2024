internal class Day08 : IDay
{
    public async Task<int> Part1(List<string> data) => GetNodes(data).antinodes;
    public async Task<int> Part2(List<string> data) => GetNodes(data).harmonics;

    private (int antinodes, int harmonics) GetNodes(List<string> grid)
    {
        var (boundY, boundX) = (grid.Count, grid.First().Length);
        var antennas = grid.SelectMany((r, y) => r.Select((_, x) => (grid[y][x], x, y))).Where(x => x.Item1 != '.').GroupBy(x => x.Item1).ToList();
        var (antinodes, harmonics) = (new HashSet<string>(), new HashSet<string>());
        foreach (var (first, second) in antennas.SelectMany(type => type.SelectMany(first => type.Where(second => second != first).Select(second => (first, second)))))
        {
            var (diffX, diffY) = (first.x - second.x, first.y - second.y);
            var (cx, cy) = (first.x + diffX, first.y + diffY);
            if (cx >= 0 && cy >= 0 && cx < boundX && cy < boundY) antinodes.Add($"{cx}:{cy}");
            while (cx >= 0 && cy >= 0 && cx < boundX && cy < boundY)
            {
                harmonics.Add($"{cx}:{cy}");
                cx += diffX;
                cy += diffY;
            }
        }
        antennas.Where(a => a.Count() > 1).SelectMany(a => a).ToList().ForEach(a => harmonics.Add($"{a.x}:{a.y}"));
        return (antinodes.Count(), harmonics.Count());
    }
}