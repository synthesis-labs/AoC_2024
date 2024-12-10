using Utilities;

namespace AdventOfCode;

public class Day10 : BaseDay
{
    StringMap<char> map;
    Dictionary<int, char> numDict = new Dictionary<int, char>() 
    { { 0, '0' }, { 1, '1' }, { 2, '2' }, { 3, '3' }, { 4, '4' },
      { 5, '5' }, { 6, '6' }, { 7, '7' }, { 8, '8' }, { 9, '9' } };

    private readonly string _input;
    public Day10()
    {
        _input = File.ReadAllText(InputFilePath);

        map = _input.AsMap();
    }
    private string ProcessInput1()
    {
        long sum = 0;

        for(var x = 0; x < map.Width; x++)
        {
            for(var y = 0; y < map.Height; y++)
            {
                if (map[(x, y)] == '0')
                {
                    sum += FindPossiblePaths(x, y, 1, 0, new HashSet<Point2D<int>>());        
                }
            }
        }

        return $"{sum}";
    }

    public int FindPossiblePaths(int x, int y, int target, int cur, HashSet<Point2D<int>> seenPeaks, bool partTwo = false)
    {
        var newDir = new Point2D<int>(x, y);

        if(cur == 9)
        {
            if(!partTwo && seenPeaks.Contains(newDir))
            {
                return 0;
            }
            seenPeaks.Add(newDir);
            return 1;
        }

        var sum = 0;
        var upwards = newDir + Point2D<int>.Up;
        if(upwards.Y > -1 && map[upwards] == numDict[target])
        {
            sum += FindPossiblePaths(upwards.X, upwards.Y, target + 1, target, seenPeaks, partTwo);
        }
        var left = newDir + Point2D<int>.Left;
        if (left.X > -1 && map[left] == numDict[target])
        {
            sum += FindPossiblePaths(left.X, left.Y, target + 1, target, seenPeaks, partTwo);
        }
        var right = newDir + Point2D<int>.Right;
        if (right.X < map.Width && map[right] == numDict[target])
        {
            sum += FindPossiblePaths(right.X, right.Y, target + 1, target, seenPeaks, partTwo);
        }
        var downwards = newDir + Point2D<int>.Down;
        if (downwards.Y < map.Height && map[downwards] == numDict[target])
        {
            sum += FindPossiblePaths(downwards.X, downwards.Y, target + 1, target, seenPeaks, partTwo);
        }

        return sum;
    }

    private string ProcessInput2()
    {
        long sum = 0;

        for (var x = 0; x < map.Width; x++)
        {
            for (var y = 0; y < map.Height; y++)
            {
                if (map[(x, y)] == '0')
                {
                    sum += FindPossiblePaths(x, y, 1, 0, new HashSet<Point2D<int>>(), true);
                }
            }
        }

        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
