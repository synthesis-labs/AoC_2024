using Utilities;

namespace AdventOfCode;

public class Day06 : BaseDay
{
    private readonly string[] _input;
    private char[][] map;
    Dictionary<(int, int), CompassDirection> seenPositions = new Dictionary<(int, int), CompassDirection>();
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
                currentDirection = currentDirection.Turn("cw");
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
        foreach (var position in seenPositions)
        {
            if (position.Key == startingPosition) continue;
            map[position.Key.Item1][position.Key.Item2] = '#';
            if (IsLoop(position.Key.Item1, position.Key.Item2, map))
            {
                sum++;
            }
            map[position.Key.Item1][position.Key.Item2] = '.';
        }

        return $"{sum}";
    }

    private bool IsLoop(int x, int y, char[][] curmap)
    {
        var visited = new HashSet<(int, int, CompassDirection)>();
        var currentPosition = startingPosition;
        var currentDirection = CompassDirection.N;
        var previous = currentPosition;

        while (true)
        {
            if (curmap[currentPosition.Item1][currentPosition.Item2] == '#')
            {
                if (!visited.Add((previous.Item1, previous.Item2, currentDirection)))
                    return true;

                currentPosition = previous;
                currentDirection = currentDirection.TurnClockwise();
            }

            previous = currentPosition;

            var nextPosition = GetNextPosition(currentPosition, currentDirection);

            if (!IsValidPosition(nextPosition.Item1, nextPosition.Item2))
            {
                return false;
            }

            currentPosition = nextPosition;
        }
    }

    private (int, int) GetNextPosition((int, int) currentPosition, CompassDirection currentDirection)
    {
        return currentDirection switch
        {
            CompassDirection.N => (currentPosition.Item1 - 1, currentPosition.Item2),
            CompassDirection.E => (currentPosition.Item1, currentPosition.Item2 + 1),
            CompassDirection.S => (currentPosition.Item1 + 1, currentPosition.Item2),
            CompassDirection.W => (currentPosition.Item1, currentPosition.Item2 - 1)
        };
    }

    private bool IsValidPosition(int x, int y)
    {
        return x > -1 && x < map.Length && y > -1 && y < map[x].Length;
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
