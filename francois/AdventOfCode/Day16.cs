using Utilities;

namespace AdventOfCode;

public class Day16 : BaseDay
{
    private readonly string _input;
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map;
    private Coordinate2D Start = new Coordinate2D(0, 0);
    private Coordinate2D End = new Coordinate2D(0, 0);
    private CompassDirection EndDir;
    private HashSet<(Coordinate2D loc, CompassDirection dir)> Seen = new HashSet<(Coordinate2D loc, CompassDirection dir)>();
    private Dictionary<(Coordinate2D loc, CompassDirection dir), int> Distances = new Dictionary<(Coordinate2D loc, CompassDirection dir), int>();

    public Day16()
    {
        _input = File.ReadAllText(InputFilePath);
        map = _input.GenerateMap(false);
        Start = map.map.First(q => q.Value == 'S').Key;
        End = map.map.First(q => q.Value == 'E').Key;
    }

    private void DistanceToLoc(Coordinate2D start, Coordinate2D end)
    {
        Distances.Clear();
        Seen.Clear();
        for (var x = 0; x < map.maxX; x++)
        {
            for(var y = 0; y < map.maxY; y++)
            {
                for(var d = 0; d < 4; d++)
                    Distances.TryAdd((new Coordinate2D(x, y), (CompassDirection)(d*90)), int.MaxValue);
            }
        }

        var moves = new PriorityQueue<(Coordinate2D loc, CompassDirection dir), int>();
        moves.Enqueue((start, CompassDirection.E), 0);
        Distances[(start, CompassDirection.E)] = 0;

        while (moves.TryDequeue(out var move, out var dist))
        {
            if (move.loc.x > -1 && move.loc.y > -1 && move.loc.x < map.maxX && move.loc.y < map.maxY)
            {
                if (map.map[move.loc] == '#' || Seen.Contains((move.loc, move.dir)))
                    continue;

                Seen.Add((move.loc, move.dir));
                Distances[(move.loc, move.dir)] = dist;

                if (move.loc == end)
                {
                    continue;
                }
                moves.Enqueue((move.loc.MoveDirection(move.dir), move.dir), dist + 1);

                var cw = move.Item2.TurnClockwise();
                var ccw = move.Item2.TurnCounterClockwise();
                moves.Enqueue((move.loc, cw), dist + 1000);
                moves.Enqueue((move.loc, ccw), dist + 1000);
            }
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        DistanceToLoc(Start, End);
        var EndLocs = Distances.Where(q => q.Key.loc == End);
        sum = EndLocs.Min(q => q.Value);
        EndDir = Distances.Single(q => q.Key.loc == End && q.Value == sum).Key.dir;
        return $"{sum}";
    }

    private void Visit(Coordinate2D cur, CompassDirection dir, Coordinate2D end, HashSet<(Coordinate2D loc, CompassDirection dir)> visited)
    {
        if (visited.Contains((cur, dir)) || cur == end)
            return;

        visited.Add((cur, dir));

        var possible = new List<(Coordinate2D loc, CompassDirection dir)>()
        {
            (cur.MoveDirection(dir.Flip()), dir),
            (cur, dir.TurnClockwise()),
            (cur, dir.TurnCounterClockwise())
        };

        var canMove = possible.Where(q => Distances.ContainsKey(q) && (
            Distances[(cur, dir)] - Distances[q] == 1 || Distances[(cur, dir)] - Distances[q] == 1000
        )).ToList();

        foreach(var move in canMove)
        {
            Visit(move.loc, move.dir, end, visited);
        }

    }

    private string ProcessInput2()
    {
        long sum = 0;
        var places = new HashSet<(Coordinate2D loc, CompassDirection dir)>();
        Visit(End, EndDir, Start, places);
        places.Add((Start, CompassDirection.E));
        sum = places.DistinctBy(q => q.loc).Count();
        return $"{sum}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());

}
