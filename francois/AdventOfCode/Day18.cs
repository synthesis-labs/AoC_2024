using Utilities;

namespace AdventOfCode;

public class Day18 : BaseDay
{
    private readonly string[] _input;
    Dictionary<Coordinate2D, char> Corruptions = new Dictionary<Coordinate2D, char>();
    Dictionary<Coordinate2D, char> map = new Dictionary<Coordinate2D, char>();
    private Queue<(Coordinate2D, int)> moves = new Queue<(Coordinate2D, int)>();
    private HashSet<Coordinate2D> visited = new HashSet<Coordinate2D>();
    private List<CompassDirection> dir = new List<CompassDirection>() { CompassDirection.N, CompassDirection.E, CompassDirection.S, CompassDirection.W };
    long Shortest = long.MaxValue;

    public Day18()
    {
        _input = File.ReadAllLines(InputFilePath);
        foreach (var line in _input)
        {
            Corruptions.Add(new Coordinate2D(line), '#');
        }
    }

    public long FindShortestPath(Coordinate2D start, Coordinate2D end)
    {
        Shortest = long.MaxValue;
        visited.Clear();
        moves.Clear();
        moves.Enqueue((start, 0));
        visited.Add(start);
        while (moves.TryDequeue(out var move))
        {
            if (end == move.Item1)
            {
                if (Shortest >= move.Item2)
                {
                    Shortest = move.Item2;
                    return move.Item2;
                }
            }

            for (var i = 0; i < dir.Count; i++)
            {
                var mov = move.Item1.MoveDirection(dir[i]);
                if (map.ContainsKey(mov) && map[mov] != '#' && !visited.Contains(mov))
                {
                    moves.Enqueue((mov, move.Item2 + 1));
                    visited.Add(mov);
                }
            }
        }
        return 0;
    }

    private string ProcessInput1(int x, int y)
    {
        long sum = 0;
        var shortCorruptions = Corruptions.Take(1024).ToDictionary();
        for (var i = 0; i <= x; i++)
        {
            for (var j = 0; j <= y; j++)
            {
                var loc = new Coordinate2D(i, j);
                if(shortCorruptions.TryGetValue(loc, out char weight))
                {
                    map.Add(loc, weight);
                }
                else
                {
                    map.Add(loc, '.');
                }
            }
        }

        var start = new Coordinate2D(0, 0);
        var end = new Coordinate2D(x, y);
        sum = FindShortestPath(start, end);
        return $"{sum}";
    }

    private string ProcessInput2(int x, int y)
    {
        long sum = 0;
        var start = new Coordinate2D(0, 0);
        var end = new Coordinate2D(x, y);
        foreach (var key in Corruptions.Keys.Skip(1024))
        {
            map[key] = '#';
            if(FindShortestPath(start, end) == 0)
            {
                return $"{key.x},{key.y}";
            }
        }
        return $"{sum}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1(70, 70));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(70, 70));


}
