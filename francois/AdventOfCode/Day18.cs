using Utilities;

namespace AdventOfCode;

public class Day18 : BaseDay
{
    private readonly string[] _input;
    char[][] map;
    private List<Coordinate2D> Corruptions = new List<Coordinate2D>();

    public Day18()
    {
        _input = File.ReadAllLines(InputFilePath);

        foreach(var line in _input)
        {
            Corruptions.Add(new Coordinate2D(line));
        }

        map = new char[73][];
        for(int x = 0; x < map.Length; x++)
        {
            map[x] = new char[73];
            Array.Fill(map[x], '.');
        }

        for (int x = 0; x < 73; x++)
            map[x][0] = map[x][73 - 1] = '#';

        for (int y = 0; y < 73; y++)
            map[0][y] = map[73 - 1][y] = '#';
    }

    private char[][] CopyMap()
    {
        var copy = new char[map.Length][];
        for(int x = 0; x < map.Length; x++)
        {
            copy[x] = new char[map[x].Length];
            for(int y = 0; y < map[x].Length; y++)
            {
                copy[x][y] = map[x][y];
            }
        }
        return copy;
    }

    public int FindShortestPath(Coordinate2D start, Coordinate2D end, char[][] curmap)
    {
        var distList = new Dictionary<Coordinate2D, int>();
        for(int x = 0; x < 73; x++)
        {
            for(int y = 0; y < 73; y++)
            {
                distList.Add(new Coordinate2D(x, y), int.MaxValue);
            }
        }
        var moves = new PriorityQueue<Coordinate2D, int>();
        var visited = new HashSet<Coordinate2D>();

        moves.Enqueue(start, Math.Abs(start.x - end.x) + Math.Abs(start.y - end.y));
        distList[start] = 0;
        while(moves.TryDequeue(out var move, out var dist))
        {
            if (move == end)
                return distList[move];

            if (curmap[move.x][move.y] == '#' || visited.Contains(move))
                continue;

            visited.Add(move);

            var neighbors = move.Neighbors();
            var nextD = distList[move] + 1;
            foreach (var neighbor in neighbors)
            {
                if (distList[neighbor] > nextD)
                {
                    distList[neighbor] = nextD;
                    moves.Enqueue(neighbor, nextD + Math.Abs(neighbor.x - end.x) + Math.Abs(neighbor.y - end.y));
                }
            }
        }
        return -1;
    }

    private void Corrupt(char[][] curmap, int places)
        => Corruptions.Take(places).ToList().ForEach(p => curmap[p.x + 1][p.y + 1] = '#');

    private string ProcessInput1(int x, int y)
    {
        long sum = 0;
        Corrupt(map, 1024);
        var start = new Coordinate2D(1, 1);
        var end = new Coordinate2D(x+1, y+1);
        sum = FindShortestPath(start, end, map);
        return $"{sum}";
    }

    private string ProcessInput2(int x, int y)
    {
        var start = new Coordinate2D(1, 1);
        var end = new Coordinate2D(x + 1, y + 1);
        var corStart = 1024 + 1;
        var corEnd = Corruptions.Count;

        while(corEnd - corStart > 1)
        {
            var half = (corStart + corEnd) / 2;
            var copy = CopyMap();
            Corrupt(copy, half);

            if(FindShortestPath(start, end, copy) == -1)
            {
                corEnd = half;
            }
            else
            {
                corStart = half;
            }
        }
        return $"{Corruptions[corStart].ToShortString()}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1(70, 70));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(70, 70));


}
