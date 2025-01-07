using Utilities;

namespace AdventOfCode;

public class Day06 : BaseDay
{
    private readonly string[] _input;
    private readonly char[][] map;
    readonly Dictionary<(int, int), CompassDirection> seenPositions = [];
    (int, int) startingPosition = new(0, 0);

    public Day06()
    {
        _input = File.ReadAllLines(InputFilePath);
        map = new char[_input.Length][];
        for (var x = 0; x < _input.Length; x++)
        {
            map[x] = new char[_input[x].Length];
            for(var y = 0; y < _input[x].Length; y++)
            {
                map[x][y] = _input[x][y];
                if (map[x][y] == '^')
                    startingPosition = new(x, y);
            }
        }
    }
    private string ProcessInput1()
    {
        var currentDirection = CompassDirection.N;
        seenPositions.Add(startingPosition, currentDirection);
        (int, int) nextPosition = new(0, 0);
        if (startingPosition.Item1 - 1 > -1)
        {
            nextPosition = (startingPosition.Item1 - 1, startingPosition.Item2);
        }
        var previous = nextPosition;
        var reachedEdge = false;
        while (!reachedEdge)
        {
            if (map[nextPosition.Item1][nextPosition.Item2] == '#')
            {
                nextPosition = previous;
                currentDirection = currentDirection.TurnClockwise();
            }
            else
            {
                seenPositions.TryAdd(nextPosition, currentDirection);
            }
            previous = nextPosition;

            nextPosition = GetNextPosition(nextPosition, currentDirection);
            
            if(!IsValidPosition(nextPosition.Item1, nextPosition.Item2))
            {
                reachedEdge = true;
            }
        }
        return $"{seenPositions.Count}";
    }

    private string ProcessInput2()
    {
        int sum = 0;
        (int, int) prevPos = startingPosition;
        CompassDirection prevDir = CompassDirection.N;
        foreach (var position in seenPositions)
        {
            if (position.Key == startingPosition) continue;
            map[position.Key.Item1][position.Key.Item2] = '#';
            if (IsLoop(prevPos, prevDir))
            {
                sum++;
            }
            map[position.Key.Item1][position.Key.Item2] = '.';
            prevPos = position.Key;
            prevDir = position.Value;
        }

        return $"{sum}";
    }

    private bool IsLoop((int, int) start, CompassDirection lastdir)
    {
        var visited = new Dictionary<(int, int), int>();
        var currentPosition = start;
        var currentDirection = lastdir;
        var previous = currentPosition;

        while (true)
        {
            if (map[currentPosition.Item1][currentPosition.Item2] == '#')
            {
                if(!visited.TryAdd(currentPosition, 1))
                {
                    if (visited[currentPosition] > 2) return true;
                    visited[currentPosition]++;
                }

                currentPosition = previous;
                currentDirection = currentDirection.TurnClockwise();
            }
            
            previous = currentPosition;

            currentPosition = GetNextPosition(currentPosition, currentDirection);

            if (!IsValidPosition(currentPosition.Item1, currentPosition.Item2))
            {
                return false;
            }
        }
    }

    private static (int, int) GetNextPosition((int, int) currentPosition, CompassDirection currentDirection)
    {
        return currentDirection switch
        {
            CompassDirection.N => (currentPosition.Item1 - 1, currentPosition.Item2),
            CompassDirection.E => (currentPosition.Item1, currentPosition.Item2 + 1),
            CompassDirection.S => (currentPosition.Item1 + 1, currentPosition.Item2),
            CompassDirection.W => (currentPosition.Item1, currentPosition.Item2 - 1),
        };
    }

    private bool IsValidPosition(int x, int y)
    {
        return x > -1 && x < map.Length && y > -1 && y < map[x].Length;
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
