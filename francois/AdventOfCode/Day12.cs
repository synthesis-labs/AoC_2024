using Utilities;

namespace AdventOfCode;

public class Day12 : BaseDay
{
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) plot;
    List<Coordinate2D> curPlots = new List<Coordinate2D>();
    HashSet<List<Coordinate2D>> knownPlots = new HashSet<List<Coordinate2D>>();
    List<Coordinate2D> seenPlots = new List<Coordinate2D>();
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
                if (!seenPlots.Contains(dir))
                {
                    curPlots = new List<Coordinate2D>();
                    FindValidPlots(plot.map[dir], dir);
                    if (curPlots.Count > 0)
                    {
                        seenPlots.AddRange(curPlots);
                        knownPlots.Add(curPlots);
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
            var seen = new List<Coordinate2D>();
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
                var neighbors = x.Neighbors().Where(q => !curPlots.Contains(q));
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
        if(!curPlots.Contains(dir))
        {
            curPlots.Add(dir);
            var upwards = dir.MoveDirection(CompassDirection.N);
            if (upwards.y <= plot.maxY && plot.map[upwards] == cur && !curPlots.Contains(upwards))
            {
                FindValidPlots(cur, upwards);
            }
            var left = dir.MoveDirection(CompassDirection.W);
            if (left.x > -1 && plot.map[left] == cur && !curPlots.Contains(left))
            {
                FindValidPlots(cur, left);
            }
            var right = dir.MoveDirection(CompassDirection.E);
            if (right.x <= plot.maxX && plot.map[right] == cur && !curPlots.Contains(right))
            {
                FindValidPlots(cur, right);
            }
            var downwards = dir.MoveDirection(CompassDirection.S);
            if (downwards.y > -1 && plot.map[downwards] == cur && !curPlots.Contains(downwards))
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
