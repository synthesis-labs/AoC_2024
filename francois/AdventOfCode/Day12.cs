using Utilities;

namespace AdventOfCode;

public class Day12 : BaseDay
{
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) plot;
    Dictionary<Coordinate2D, char> curPlots = new Dictionary<Coordinate2D, char>();
    HashSet<HashSet<Coordinate2D>> knownPlots = new HashSet<HashSet<Coordinate2D>>();
    Dictionary<Coordinate2D, char> seenPlots = new Dictionary<Coordinate2D, char>();
    private readonly string _input;
    public Day12()
    {
        _input = File.ReadAllText(InputFilePath);
        plot = _input.GenerateMap();
    }
    private string ProcessInput1(string input)
    {
        long sum = 0;
        for (var x = 0; x <= plot.maxX; x++)
        {
            for (var y = 0; y <= plot.maxY; y++)
            {
                var dir = new Coordinate2D(x, y);
                if (seenPlots.TryAdd(dir, plot.map[dir]))
                {
                    curPlots = new Dictionary<Coordinate2D, char>();
                    FindValidPlots(plot.map[dir], dir);
                    if (curPlots.Count > 0)
                    {
                        foreach (var places in curPlots)
                        {
                            seenPlots.TryAdd(places.Key, places.Value);
                        }
                        knownPlots.Add(curPlots.KeyList().ToHashSet());
                        sum += CalculateBoundary();
                    }
                }
            }
        }
        return $"{sum}";
    }

    private int CalculateBoundary(bool partTwo = false)
    {
        if(partTwo)
        {
            var total = 0;
            foreach (var plots in knownPlots)
            {
                var minx = plots.Min(q => q.x);
                var miny = plots.Min(q => q.y);
                var maxx = plots.Max(q => q.x);
                var maxy = plots.Max(q => q.y);
                var sides = 0;
                for (int x = minx; x <= maxx; x++)
                {
                    for (int y = miny; y <= maxy; y++)
                    {
                        var dir = (x, y);
                        if (plots.Contains(dir))
                        {
                            if (!plots.Contains(dir.MoveDirection(CompassDirection.W)))
                            {
                                if (!plots.Contains(dir.MoveDirection(CompassDirection.S)))
                                    sides++;
                                else if (plots.Contains(dir.MoveDirection(CompassDirection.SW)))
                                    sides++;
                            }

                            if (!plots.Contains(dir.MoveDirection(CompassDirection.S)))
                            {
                                if (!plots.Contains(dir.MoveDirection(CompassDirection.W)))
                                    sides++;
                                else if (plots.Contains(dir.MoveDirection(CompassDirection.SW)))
                                    sides++;
                            }

                            if (!plots.Contains(dir.MoveDirection(CompassDirection.E)))
                            {
                                if (!plots.Contains(dir.MoveDirection(CompassDirection.S)))
                                    sides++;
                                else if (plots.Contains(dir.MoveDirection(CompassDirection.SE)))
                                    sides++;
                            }

                            if (!plots.Contains(dir.MoveDirection(CompassDirection.N)))
                            {
                                if (!plots.Contains(dir.MoveDirection(CompassDirection.W)))
                                    sides++;
                                else if (plots.Contains(dir.MoveDirection(CompassDirection.NW)))
                                    sides++;
                            }
                        }
                    }
                }
                total += sides * plots.Count;
            }
            return total;
        }
        else
        {
            var sides = 0;
            foreach (var x in curPlots)
            {
                var neighbors = x.Key.Neighbors().Where(q => !curPlots.TryGetValue(q, out char _));
                foreach(var y in neighbors)
                {
                    sides++;
                }
            }
            return sides * curPlots.Count;
        }
    }

    private void FindValidPlots(char cur, Coordinate2D dir)
    {
        if(curPlots.TryAdd(dir, plot.map[dir]))
        {
            var upwards = dir.MoveDirection(CompassDirection.N);
            if (upwards.y <= plot.maxY && plot.map[upwards] == cur)
            {
                FindValidPlots(cur, upwards);
            }
            var left = dir.MoveDirection(CompassDirection.W);
            if (left.x > -1 && plot.map[left] == cur)
            {
                FindValidPlots(cur, left);
            }
            var right = dir.MoveDirection(CompassDirection.E);
            if (right.x <= plot.maxX && plot.map[right] == cur)
            {
                FindValidPlots(cur, right);
            }
            var downwards = dir.MoveDirection(CompassDirection.S);
            if (downwards.y > -1 && plot.map[downwards] == cur)
            {
                FindValidPlots(cur, downwards);
            }
        }
    }

    private string ProcessInput2(string input)
    {
        long sum = 0;
        sum += CalculateBoundary(true);
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
