using Utilities;

namespace AdventOfCode;

public class Day06 : BaseDay
{
    private readonly string[] _input;
    Dictionary<(int, int), CompassDirection> seenPositions = new Dictionary<(int, int), CompassDirection>();
    List<(int, int, CompassDirection)> newRoute = new List<(int, int, CompassDirection)>();
    (int, int) startingPosition = new(0, 0);

    public Day06()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var x = 0; x < _input.Length - 1; x++)
        {
            for (var y = 0; y < _input.Length - 1; y++)
            {
                if (_input[x][y] == '^')
                {
                    startingPosition = (x, y);
                }
            }
        }
    }
    private string ProcessInput1(string[] input)
    {
        var currentDirection = CompassDirection.N;
        int sum = 0;
        seenPositions.Add(startingPosition, currentDirection);
        (int, int) nextPosition = new(0, 0);
        if(startingPosition.Item1 - 1 > -1)
        {
            nextPosition = (startingPosition.Item1 -1, startingPosition.Item2);
        }
        var reachedEdge = false;
        while (!reachedEdge)
        {
            if (input[nextPosition.Item1][nextPosition.Item2] == '#')
            {
                switch (currentDirection)
                {
                    case CompassDirection.N:
                        nextPosition.Item1 += 1;
                        currentDirection = CompassDirection.E;
                        break;
                    case CompassDirection.E:
                        nextPosition.Item2 -= 1;
                        currentDirection = CompassDirection.S;
                        break;
                    case CompassDirection.S:
                        nextPosition.Item1 -= 1;
                        currentDirection = CompassDirection.W;
                        break;
                    case CompassDirection.W:
                        nextPosition.Item2 += 1;
                        currentDirection = CompassDirection.N;
                        break;
                }
            }
            else
            {
                seenPositions.TryAdd(nextPosition, currentDirection);
            }
            switch (currentDirection)
            {
                case CompassDirection.N:
                    if (nextPosition.Item1 - 1 > -1)
                        nextPosition = new(nextPosition.Item1 - 1, nextPosition.Item2);
                    else reachedEdge = true;
                    break;
                case CompassDirection.E:
                    if (nextPosition.Item2 + 1 < input[nextPosition.Item1].Length)
                        nextPosition = new(nextPosition.Item1, nextPosition.Item2 + 1);
                    else reachedEdge = true;
                    break;
                case CompassDirection.S:
                    if (nextPosition.Item1 + 1 < input.Length)
                        nextPosition = new(nextPosition.Item1 + 1, nextPosition.Item2);
                    else reachedEdge = true;
                    break;
                case CompassDirection.W:
                    if (nextPosition.Item2 - 1 > -1)
                        nextPosition = new(nextPosition.Item1, nextPosition.Item2 - 1);
                    else reachedEdge = true;
                    break;
            }
        }

        sum = seenPositions.Count;
        return $"{sum}";
    }
    private string ProcessInput2(string[] input)
    {
        int sum = 0;
        foreach(var pos in seenPositions)
        {
            if (pos.Key == startingPosition) continue;
            newRoute = new List<(int, int, CompassDirection)>();
            var testset = new List<string>(input);
            testset[pos.Key.Item1] = testset[pos.Key.Item1].Remove(pos.Key.Item2, 1);
            if (testset[pos.Key.Item1].Length < pos.Key.Item2)
            {
                testset[pos.Key.Item1] = testset[pos.Key.Item1].Append('#').ToString();
            }
            else
            {
                testset[pos.Key.Item1] = testset[pos.Key.Item1].Insert(pos.Key.Item2, "#");
            }
            var currentDirection = seenPositions[startingPosition];
            newRoute.Add(new(startingPosition.Item1, startingPosition.Item2, currentDirection));
            var reachedEdge = false;
            (int, int) nextPosition = new (0, 0);
            if (startingPosition.Item1 - 1 > -1)
            {
                nextPosition = (startingPosition.Item1 - 1, startingPosition.Item2);
            }
            var steppedBack = false;
            while (!reachedEdge)
            {
                if (newRoute.Any(q => q.Item1 == nextPosition.Item1 && q.Item2 == nextPosition.Item2 && q.Item3 == currentDirection))
                {
                    break;
                }
                if (testset[nextPosition.Item1][nextPosition.Item2] == '#')
                {
                    switch (currentDirection)
                    {
                        case CompassDirection.N:
                            nextPosition.Item1 += 1;
                            currentDirection = CompassDirection.E;
                            break;
                        case CompassDirection.E:
                            nextPosition.Item2 -= 1;
                            currentDirection = CompassDirection.S;
                            break;
                        case CompassDirection.S:
                            nextPosition.Item1 -= 1;
                            currentDirection = CompassDirection.W;
                            break;
                        case CompassDirection.W:
                            nextPosition.Item2 += 1;
                            currentDirection = CompassDirection.N;
                            break;
                    }
                }
                else
                {
                    if(newRoute.Contains(new(nextPosition.Item1, nextPosition.Item2, currentDirection)))
                    {
                        break;
                    }
                    else
                    {
                        newRoute.Add(new(nextPosition.Item1, nextPosition.Item2, currentDirection));
                    }
                }
                switch (currentDirection)
                {
                    case CompassDirection.N:
                        if (nextPosition.Item1 - 1 > -1)
                            nextPosition = new(nextPosition.Item1 - 1, nextPosition.Item2);
                        else reachedEdge = true;
                        break;
                    case CompassDirection.E:
                        if (nextPosition.Item2 + 1 < testset[nextPosition.Item1].Length)
                            nextPosition = new(nextPosition.Item1, nextPosition.Item2 + 1);
                        else reachedEdge = true;
                        break;
                    case CompassDirection.S:
                        if (nextPosition.Item1 + 1 < testset.Count)
                            nextPosition = new(nextPosition.Item1 + 1, nextPosition.Item2);
                        else reachedEdge = true;
                        break;
                    case CompassDirection.W:
                        if (nextPosition.Item2 - 1 > -1)
                            nextPosition = new(nextPosition.Item1, nextPosition.Item2 - 1);
                        else reachedEdge = true;
                        break;
                }
            }

            if(!reachedEdge)
            {
                sum++;
            }
        }

        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
