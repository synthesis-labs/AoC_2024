namespace AdventOfCode;

public class Day08 : BaseDay
{
    private HashSet<(int, int, char)> values = new HashSet<(int, int, char)>();
    private Dictionary<(int, int), char> antinodes = new Dictionary<(int, int), char>();
    private readonly string[] _input;
    public Day08()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var x = 0; x < _input.Length; x++)
        {
            for(var y = 0; y < _input[x].Length; y++)
            {
                if (_input[x][y] != '.')
                {
                    values.Add(new (x, y, _input[x][y]));
                }
            }
        }
    }
    private string ProcessInput1(string[] input)
    {
        long sum = 0;
        var groups = values.GroupBy(q => q.Item3);

        foreach(var group in groups)
        {
            foreach(var x in group)
            {
                var compgroup = group.Where(q => q != x);

                foreach(var y in compgroup)
                {
                    var xmove = x.Item1 - y.Item1;
                    var ymove = x.Item2 - y.Item2;

                    if (CheckBounds(x.Item1 + xmove, x.Item2 + ymove, input)
                        && !antinodes.ContainsKey((x.Item1 + xmove, x.Item2 + ymove)))
                        antinodes.Add((x.Item1 + xmove, x.Item2 + ymove), input[x.Item1 + xmove][x.Item2 + ymove]);
                    if (CheckBounds(y.Item1 - xmove, y.Item2 - ymove, input)
                        && !antinodes.ContainsKey((y.Item1 - xmove, y.Item2 - ymove)))
                        antinodes.Add((y.Item1 - xmove, y.Item2 - ymove), input[y.Item1 - xmove][y.Item2 - ymove]);
                }
            }
        }
        sum = antinodes.Count();
        return $"{sum}";
    }

    private bool CheckBounds(int x, int y, string[] input)
    {
        return (x > -1 && y > -1 && x < input.Length && y < input[x].Length);
    }

    private string ProcessInput2(string[] input)
    {
        long sum = 0;
        var groups = values.GroupBy(q => q.Item3);

        foreach (var group in groups)
        {
            foreach (var x in group)
            {
                var compgroup = group.Where(q => q != x);

                foreach (var y in compgroup)
                {
                    var xmove = x.Item1 - y.Item1;
                    var ymove = x.Item2 - y.Item2;

                    var lastx = xmove;
                    var lasty = ymove;
                    var outboundsx = false;
                    var outboundsy = false;
                    while (!outboundsx || !outboundsy)
                    {
                        lastx += xmove;
                        lasty += ymove;

                        if (!outboundsx && CheckBounds(x.Item1 + lastx, x.Item2 + lasty, input))
                        {
                            if (!antinodes.ContainsKey((x.Item1 + lastx, x.Item2 + lasty)))
                                antinodes.Add((x.Item1 + lastx, x.Item2 + lasty), input[x.Item1 + lastx][x.Item2 + lasty]);
                        }
                        else outboundsx = true;
                        if (!outboundsy && CheckBounds(y.Item1 - lastx, y.Item2 - lasty, input))
                        {
                            if (!antinodes.ContainsKey((y.Item1 - lastx, y.Item2 - lasty)))
                                antinodes.Add((y.Item1 - lastx, y.Item2 - lasty), input[y.Item1 - lastx][y.Item2 - lasty]);
                        }
                        else outboundsy = true;
                    }
                    if (!antinodes.ContainsKey((x.Item1, x.Item2)))
                        antinodes.Add((x.Item1, x.Item2), x.Item3);
                }
            }
        }
        sum = antinodes.Count();
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
