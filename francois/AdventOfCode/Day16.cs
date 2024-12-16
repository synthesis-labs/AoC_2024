using Spectre.Console;
using Utilities;

namespace AdventOfCode;

public class Day16 : BaseDay
{
    private readonly string _input;
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map;
    private Coordinate2D Start = new Coordinate2D(0, 0);
    private Coordinate2D End = new Coordinate2D(0, 0);
    private int Shortest = int.MaxValue;
    private Queue<(Coordinate2D, CompassDirection, int, List<Coordinate2D>)> moves = new Queue<(Coordinate2D, CompassDirection, int, List<Coordinate2D>)>();
    private Dictionary<(Coordinate2D, CompassDirection), int> seen = new Dictionary<(Coordinate2D, CompassDirection), int>();
    private Dictionary<long, List<Coordinate2D>> Routes = new Dictionary<long, List<Coordinate2D>>();
    public Day16()
    {
        _input = File.ReadAllText(InputFilePath);
        map = _input.GenerateMap(false);
        Start = map.map.First(q => q.Value == 'S').Key;
        End = map.map.First(q => q.Value == 'E').Key;
    }

    private void ShortestPath(Coordinate2D cur, CompassDirection dir, int visited, List<Coordinate2D> seenRoutes)
    {
        //if (seenRoutes.Contains(cur)) return;
        //if (map.map[cur] == '#') return;
        if (cur == End)
        {
            if (visited < Shortest)
            {
                Shortest = visited;
            }
            return;
        }

        var move = cur.MoveDirection(dir);
        ShortestPath(move, dir, visited + 1, seenRoutes);
        var cw = TurnClockwise(dir);
        ShortestPath(cur.MoveDirection(cw), cw, visited + 1001, seenRoutes);
        var ccw = TurnCounterClockwise(dir);
        ShortestPath(cur.MoveDirection(ccw), ccw, visited + 1001, seenRoutes);
    }
    private string ProcessInput1()
    {
        long sum = 0;
        //ShortestPath(Start, CompassDirection.N, 0, new List<Coordinate2D>());
        moves.Enqueue((Start, CompassDirection.E, 0, new List<Coordinate2D>() { Start }));

        while (moves.TryDequeue(out var move))
        {
            if(move.Item1.x > -1 &&  move.Item1.y > -1 && move.Item1.x < map.maxX && move.Item1.y < map.maxX)
            {
                if(move.Item3 > Shortest)
                {
                    continue;
                }

                if (map.map[move.Item1] == 'E')
                {
                    if(move.Item3 <= Shortest)
                    {
                        Shortest = move.Item3;
                        if(!Routes.TryAdd(move.Item3, move.Item4))
                        {
                            Routes[move.Item3] = Routes[move.Item3].Union(move.Item4).Distinct().ToList();
                        }
                    }
                    continue;
                }

                if (map.map[move.Item1] != '#')
                {
                    var movement = move.Item1.MoveDirection(move.Item2);
                    var newlist = new List<Coordinate2D>(move.Item4);
                    newlist.Add(movement);
                    addIfShorter((movement, move.Item2, move.Item3 + 1, newlist));
                }
                var cw = TurnClockwise(move.Item2);
                var ccw = TurnCounterClockwise(move.Item2);
                addIfShorter((move.Item1, cw, move.Item3 + 1000, move.Item4));
                addIfShorter((move.Item1, ccw, move.Item3 + 1000, move.Item4));
            }
            else
            {
                continue;
            }
        }

        sum = Shortest;
        return $"{sum}";
    }

    private void addIfShorter((Coordinate2D, CompassDirection, int, List<Coordinate2D>) cur)
    {
        if(!seen.TryAdd((cur.Item1, cur.Item2), cur.Item3))
        {
            if (seen[(cur.Item1, cur.Item2)] >= cur.Item3)
            {
                seen[(cur.Item1, cur.Item2)] = cur.Item3;
                moves.Enqueue(cur);
            }
        }
        else
        {
            moves.Enqueue(cur);
        }
    }

    private string ProcessInput2()
    {
        long sum = 0; 
        sum = Routes[Routes.Keys.Min()].Distinct().Count();
        return $"{sum}";
    }
    private CompassDirection TurnClockwise(CompassDirection currentDirection)
    {
        return currentDirection switch
        {
            CompassDirection.N => CompassDirection.E,
            CompassDirection.E => CompassDirection.S,
            CompassDirection.S => CompassDirection.W,
            CompassDirection.W => CompassDirection.N,
        };
    }

    private CompassDirection TurnCounterClockwise(CompassDirection currentDirection)
    {
        return currentDirection switch
        {
            CompassDirection.N => CompassDirection.W,
            CompassDirection.E => CompassDirection.N,
            CompassDirection.S => CompassDirection.E,
            CompassDirection.W => CompassDirection.S,
        };
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());

    public class Path
    {
        public char path { get; set; }
        public int visited { get; set; }
    }
}
