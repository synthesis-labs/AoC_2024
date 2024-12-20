using Utilities;

namespace AdventOfCode;

public class Day20 : BaseDay
{
    private readonly string _input;
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map;
    private Dictionary<Coordinate2D, int> moves = new Dictionary<Coordinate2D, int>();
    Coordinate2D start = new Coordinate2D(0, 0);
    Coordinate2D end = new Coordinate2D(0, 0);

    public Day20()
    {
        _input = File.ReadAllText(InputFilePath);

        map = _input.GenerateMap(false);

        start = map.map.First(q => q.Value == 'S').Key;
        end = map.map.First(q => q.Value == 'E').Key;

        var cur = start;
        int count = 0;
        moves[start] = 0;

        while (cur != end)
        {
            var next = cur.Neighbors().FirstOrDefault(q => map.map[q] != '#' &&
                    !moves.ContainsKey(q));
            count++;
            moves[next] = count;
            cur = next;
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        foreach (Coordinate2D move in moves.Keys)
        {
            var dist = moves.Where(q => q.Key.ManDistance(move) <= 2).ToHashSet();
            foreach (var pos in dist)
            {
                if (moves.ContainsKey(pos.Key))
                {
                    int time = pos.Value - moves[move] - pos.Key.ManDistance(move);
                    if (time >= 100) sum++;
                }
            }
        }
        //blocks.Aggregate((x, y) =>
        //{
        //    map.map[x] = '.';
        //    map.map[y] = '.';
        //    var test = FindShortestPath(start, end);
        //    if (test < Shortest && Shortest - test > 101)
        //    {
        //        sum++;
        //    }

        //    map.map[x] = '#';
        //    map.map[y] = '#';

        //    return x;
        //});
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        foreach (Coordinate2D move in moves.Keys)
        {
            var dist = moves.Where(q => q.Key.ManDistance(move) <= 20).ToHashSet();
            foreach (var pos in dist)
            {
                if (moves.ContainsKey(pos.Key))
                {
                    int time = pos.Value - moves[move] - pos.Key.ManDistance(move);
                    if (time >= 100) sum++;
                }
            }
        }
        return $"{sum}";
    }

    
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
