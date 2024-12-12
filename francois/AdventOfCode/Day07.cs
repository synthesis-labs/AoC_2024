using Utilities;

namespace AdventOfCode;

public class Day07 : BaseDay
{
    private readonly HashSet<(long, List<long>)> testValues = new HashSet<(long, List<long>)>();
    private readonly string[] _input;
    public Day07()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var x = 0; x < _input.Length; x++)
        {
            var split = _input[x].Split(':');
            testValues.Add((long.Parse(split[0]), split[1].ToLongList(" ")));
        }
    }
    private string ProcessInput1(string[] input)
    {
        long sum = 0;
        foreach (var value in testValues)
        {
            var isValid = CheckValidity(value.Item1, value.Item2, 1, false, value.Item2[0]);

            if (isValid)
            {
                sum += value.Item1;
            }
        }
        return $"{sum}";
    }
    private string ProcessInput2(string[] input)
    {
        long sum = 0;
        foreach (var value in testValues)
        {
            var isValid = CheckValidity(value.Item1, value.Item2, 1, true, value.Item2[0]);

            if (isValid)
            {
                sum += value.Item1;
            }
        }
        return $"{sum}";
    }

    private bool CheckValidity(long target, List<long> numbers, int index, bool partTwo, long cur = 0)
    {
        if (index == numbers.Count)
        {
            return target == cur;
        }

        if (CheckValidity(target, numbers, index + 1, partTwo, cur + numbers[index])) return true;

        if (CheckValidity(target, numbers, index + 1, partTwo, cur * numbers[index])) return true;

        if (partTwo)
        {
            if(index < numbers.Count)
            {
                cur = long.Parse($"{cur}{numbers[index]}");
                if (CheckValidity(target, numbers, index + 1, partTwo, cur))
                    return true;
            }
        }

        return false;

    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
