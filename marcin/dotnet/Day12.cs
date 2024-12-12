internal class Day12: IDay
{
    public double Part1(List<string> data) => CalculateFenceCost(data).price;
    public double Part2(List<string> data) => CalculateFenceCost(data).budget;
    
    private HashSet<Plant> TraverseRegion(Plant? plant)
    {
        var region = new HashSet<Plant>();
        if (plant == null) 
            return region;
        plant.Visited = true;
        region.Add(plant);
        foreach (var neighbour in plant.Neighbours.Where(n => !n.Visited).ToList())
            foreach (var map in TraverseRegion(neighbour))
                region.Add(map);
        return region;
    }

    private int DistinctSides(IEnumerable<IGrouping<int, Plant>> edge, bool topDown)
    {
        var side = 0;
        foreach (var group in edge)
        {
            side++;
            var o = topDown
                ? group.Select(p => p.Position.c).OrderBy(p => p) 
                : group.Select(p => p.Position.r).OrderBy(p => p);
            for (int x = 0; x < o.Count() - 1; x++)
                side += (o.ElementAt(x + 1) - o.ElementAt(x) > 1) ? 1 : 0;
        }
        return side;
    }

    private (int price, int budget) CalculateFenceCost(List<string> data)
    {
        var garden = data.Select((d, r) => d.Select((d, c) => new Plant { Species = d, Position = (r, c), Top = r, Bottom = r + 1, Left = c, Right = c + 1 }).ToList()).ToList();
        for (int r = 0; r < garden.Count(); r++)
            for (int c = 0; c < garden.First().Count(); c++)
            {
                if (r > 0 && garden[r - 1][c].Species == garden[r][c].Species)
                {
                    garden[r][c].Neighbours.Add(garden[r - 1][c]);
                    garden[r][c].Perimiter--;
                    garden[r][c].Top = -1;
                }
                if (r < garden.Count() - 1 && garden[r + 1][c].Species == garden[r][c].Species)
                {
                    garden[r][c].Neighbours.Add(garden[r + 1][c]);
                    garden[r][c].Perimiter--;
                    garden[r][c].Bottom = -1;
                }
                if (c > 0 && garden[r][c - 1].Species == garden[r][c].Species)
                {
                    garden[r][c].Neighbours.Add(garden[r][c - 1]);
                    garden[r][c].Perimiter--;
                    garden[r][c].Left = -1;
                }
                if (c < garden.First().Count() - 1 && garden[r][c + 1].Species == garden[r][c].Species)
                {
                    garden[r][c].Neighbours.Add(garden[r][c + 1]);
                    garden[r][c].Perimiter--;
                    garden[r][c].Right = -1;
                }
            }

        var plants = garden.SelectMany(p => p).ToList();
        Plant? plant = null;

        (int price, int budget) = (0, 0);
        do
        {
            plant = plants.FirstOrDefault(p => !p.Visited);
            var region = TraverseRegion(plant);
            var area = region.Count();
            var perimiter = region.Sum(r => r.Perimiter);
            int top = DistinctSides(region.Where(p => p.Top >= 0).GroupBy(p => p.Top), true);
            int bottom = DistinctSides(region.Where(p => p.Bottom >= 0).GroupBy(p => p.Bottom), true);
            int left = DistinctSides(region.Where(p => p.Left >= 0).GroupBy(p => p.Left), false);
            int right = DistinctSides(region.Where(p => p.Right >= 0).GroupBy(p => p.Right), false);
            var edges = (top + bottom + left + right);
            price += (region.Count() * perimiter);
            budget += (area * edges);
        } while (plants.Where(p => !p.Visited).Count() > 0);

        return (price, budget);
    }

    private class Plant
    {
        public char Species { get; set; }
        public int Perimiter { get; set; } = 4;
        public (int r, int c) Position { get; set; }
        public bool Visited { get; set; }
        public int Top { get; set; }
        public int Bottom { get; set; }
        public int Left { get; set; }
        public int Right { get; set; }
        public HashSet<Plant> Neighbours { get; set; } = new HashSet<Plant>();
    }
}