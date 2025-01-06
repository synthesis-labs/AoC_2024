using System.Numerics;
using Utilities;

namespace AdventOfCode;

public class Day20 : BaseDay
{
    private readonly string _input;
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map;
    Dictionary<Coordinate2D, int> moves = new Dictionary<Coordinate2D, int>();
    Coordinate2D start = new Coordinate2D(0, 0);
    Coordinate2D end = new Coordinate2D(0, 0);

    public Day20()
    {
        _input = File.ReadAllText(InputFilePath);

        map = _input.GenerateMap(false);

        start = map.map.First(q => q.Value == 'S').Key;
        end = map.map.First(q => q.Value == 'E').Key;
    }

    private int ReachableCoords(Coordinate2D cur, int dist)
    {
        int total = 0;
        for(int x = -dist; x <= dist; x++)
        {
            int remain = dist - Math.Abs(x);
            for(int y = -remain; y <= remain; y++)
            {
                if (x == 0 && y == 0)
                    continue;

                var newpos = (cur.x + x, cur.y + y);
                if (IsValidPosition(newpos.Item1, newpos.Item2) && map.map[newpos] != '#')
                {
                    var diff = newpos.ManhattanDistance(cur);
                    int time = moves[cur] - moves.GetValueOrDefault(newpos, int.MaxValue) - diff;
                    if (time >= 100) total++;
                }
                    
            }
        }
        return total;
    }

    private void BuildRoute()
    {
        var cur = start;
        int count = 0;
        moves[start] = 0;
        while (cur != end)
        {
            var next = cur.Neighbors().FirstOrDefault(q => map.map[q] != '#' &&
                    !moves.ContainsKey(q));
            moves[next] = ++count;
            cur = next;
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        BuildRoute();
        foreach (var move in moves.Keys)
        {
            sum += ReachableCoords(move, 2);
        }
        
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        foreach (var move in moves.Keys)
        {
            sum += ReachableCoords(move, 20);
        }
        return $"{sum}";
    }

    private bool IsValidPosition(int x, int y)
    {
        return x > -1 && x < map.maxX && y > -1 && y < map.maxY;
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
