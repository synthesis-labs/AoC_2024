using Utilities;

namespace AdventOfCode;

public class Day06 : BaseDay
{
    private readonly string[] _input;
    Dictionary<(int, int), CompassDirection> seenPositions = new Dictionary<(int, int), CompassDirection>();
    (int, int) startingPosition = new(0, 0);

    public Day06()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var x = 0; x < _input.Length - 1; x++)
        {
            var y = _input[x].IndexOf('^');
            if (y >= 0)
            {
                startingPosition = new(x, y);
                break;
            }
        }
    }

    private string ProcessInput1(string[] input)
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
            if (input[nextPosition.Item1][nextPosition.Item2] == '#')
            {
                nextPosition = previous;
                currentDirection = TurnClockwise(currentDirection);
            }
            else
            {
                seenPositions.TryAdd(nextPosition, currentDirection);
            }
            previous = nextPosition;

            nextPosition = GetNextPosition(nextPosition, currentDirection);
            
            if(!IsValidPosition(nextPosition.Item1, nextPosition.Item2, input))
            {
                reachedEdge = true;
            }
        }
        return $"{seenPositions.Count}";
    }

    private string ProcessInput2(string[] input)
    {
        int sum = 0;
        foreach (var position in seenPositions)
        {
            if (position.Key == startingPosition) continue;
            if (IsLoop(position.Key.Item1, position.Key.Item2, input))
            {
                sum++;
            }
        }

        return $"{sum}";
    }

    private bool IsLoop(int x, int y, string[] input)
    {
        var visited = new HashSet<(int, int, CompassDirection)>();
        var currentPosition = startingPosition;
        var currentDirection = CompassDirection.N;
        var previous = currentPosition;

        var isLoop = false;
        while (!isLoop)
        {
            if (visited.Contains((currentPosition.Item1, currentPosition.Item2, currentDirection)))
                isLoop = true;


            if (input[currentPosition.Item1][currentPosition.Item2] == '#' ||
                (currentPosition.Item1 == x && currentPosition.Item2 == y))
            {
                visited.Add((previous.Item1, previous.Item2, currentDirection));
                currentPosition = previous;
                currentDirection = TurnClockwise(currentDirection);
            }

            previous = currentPosition;

            if (currentDirection == CompassDirection.N || currentDirection == CompassDirection.S)
            {
                var testPosition = GetNextPosition(currentPosition, currentDirection);
                if (!IsValidPosition(testPosition.Item1, testPosition.Item2, input))
                {
                    break;
                }
                currentPosition = testPosition;
            }
            else if (currentDirection == CompassDirection.E)
            {
                var nextPosition = input[currentPosition.Item1].AllIndexesOf("#");
                var hash = nextPosition.Order().LastOrDefault(q => q > currentPosition.Item2);
                if (hash > 0 && currentPosition.Item1 != x)
                {
                    (int, int) next = new(currentPosition.Item1, hash);
                    previous = (next.Item1, next.Item2 - 1);
                    currentPosition = next;
                }
                else
                {
                    var testPosition = GetNextPosition(currentPosition, currentDirection);
                    if (!IsValidPosition(testPosition.Item1, testPosition.Item2, input))
                    {
                        break;
                    }
                    currentPosition = testPosition;
                }
            }
            else
            {
                var nextPosition = input[currentPosition.Item1].AllIndexesOf("#");
                var hash = nextPosition.Order().FirstOrDefault(q => q < currentPosition.Item2);
                if (hash > 0 && currentPosition.Item1 != x)
                {
                    (int, int) next = new(currentPosition.Item1, hash);
                    previous = (next.Item1, next.Item2 + 1);
                    currentPosition = next;
                }
                else
                {
                    var testPosition = GetNextPosition(currentPosition, currentDirection);
                    if (!IsValidPosition(testPosition.Item1, testPosition.Item2, input))
                    {
                        break;
                    }
                    currentPosition = testPosition;
                }
            }

        }
        return isLoop;
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

    private bool IsValidPosition(int x, int y, string[] input)
    {
        return x > -1 && x < input.Length && y > -1 && y < input[x].Length;
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
