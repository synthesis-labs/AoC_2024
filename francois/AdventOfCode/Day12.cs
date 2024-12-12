using Utilities;

namespace AdventOfCode;

public class Day12 : BaseDay
{
    StringMap<char> plot;
    List<Point2D<int>> curPlots = new List<Point2D<int>>();
    HashSet<List<Point2D<int>>> knownPlots = new HashSet<List<Point2D<int>>>();
    List<Point2D<int>> seenPlots = new List<Point2D<int>>();
    private readonly string _input;
    public Day12()
    {
        _input = File.ReadAllText(InputFilePath);
        plot = _input.AsMap();
    }
    private string ProcessInput1(string input)
    {
        long sum = 0;
        for (var x = 0; x < plot.Width; x++)
        {
            for (var y = 0; y < plot.Height; y++)
            {
                var dir = (x, y);
                curPlots = new List<Point2D<int>>();
                if (!seenPlots.Contains(dir))
                {
                    var hasValid = FindValidPlots(plot[dir], dir);
                    if (curPlots.Count > 0 && hasValid)
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
            var seen = new List<Point2D<int>>();
            var total = 0;
            foreach (var plots in knownPlots)
            {
                var minx = plots.Min(q => q.X);
                var miny = plots.Min(q => q.Y);
                var maxx = plots.Max(q => q.X);
                var maxy = plots.Max(q => q.Y);
                var sides = 0;
                for(int x = minx; x <= maxx; x++)
                {
                    for(int y = miny; y <= maxy; y++)
                    {
                        var dir = (x, y);
                        if (plots.Contains(dir))
                        {
                            if(!plots.Contains(dir + Point2D<int>.Left))
                            {
                                if (!plots.Contains(dir + Point2D<int>.Up))
                                    sides++;
                                else if(plots.Contains(dir + Point2D<int>.Up + Point2D<int>.Left))
                                    sides++;
                            }

                            if(!plots.Contains(dir + Point2D<int>.Up))
                            {
                                if (!plots.Contains(dir + Point2D<int>.Left))
                                    sides++;
                                else if (plots.Contains(dir + Point2D<int>.Up + Point2D<int>.Left))
                                    sides++;
                            }

                            if (!plots.Contains(dir + Point2D<int>.Right))
                            {
                                if (!plots.Contains(dir + Point2D<int>.Up))
                                    sides++;
                                else if (plots.Contains(dir + Point2D<int>.Right + Point2D<int>.Up))
                                    sides++;
                            }

                            if (!plots.Contains(dir + Point2D<int>.Down))
                            {
                                if (!plots.Contains(dir + Point2D<int>.Left))
                                    sides++;
                                else if (plots.Contains(dir + Point2D<int>.Left + Point2D<int>.Down))
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
                var neighbours = x.Neighbours();
                foreach(var y in neighbours)
                {
                    if (!curPlots.Contains(y))
                        sides++;
                }
            }
            return sides * curPlots.Count;
        }
    }

    private bool FindValidPlots(char cur, Point2D<int> dir)
    {
        if(!curPlots.Contains(dir))
        {
            curPlots.Add(dir);
            var upwards = dir + Point2D<int>.Up;
            if (!curPlots.Contains(upwards) && upwards.Y > -1 && plot[upwards] == cur)
            {
                FindValidPlots(cur, upwards);
            }
            var left = dir + Point2D<int>.Left;
            if (!curPlots.Contains(left) && left.X > -1 && plot[left] == cur)
            {
                FindValidPlots(cur, left);
            }
            var right = dir + Point2D<int>.Right;
            if (!curPlots.Contains(right) && right.X < plot.Width && plot[right] == cur)
            {
                FindValidPlots(cur, right);
            }
            var downwards = dir + Point2D<int>.Down;
            if (!curPlots.Contains(downwards) && downwards.Y < plot.Height && plot[downwards] == cur)
            {
                FindValidPlots(cur, downwards);
            }
            return true;
        }
        else
        {
            return false;
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
