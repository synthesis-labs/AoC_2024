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
            moves[next] = ++count;
            cur = next;
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        var group = MainUtilities.ManDistGroup(2);
        foreach (var move in moves.Keys)
        {
            foreach(var spot in group)
            {
                var moved = move + spot;
                if (moves.ContainsKey(move + spot))
                {
                    int time = moves[moved] - moves[move] - moved.ManDistance(move);
                    if (time >= 100) sum++;
                }
            }
        }
        
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        var group = MainUtilities.ManDistGroup(20);
        foreach (var move in moves.Keys)
        {
            foreach (var spot in group)
            {
                var moved = move + spot;
                if (moves.ContainsKey(move + spot))
                {
                    int time = moves[moved] - moves[move] - moved.ManDistance(move);
                    if (time >= 100) sum++;
                }
            }
        }
        return $"{sum}";
    }

    
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
