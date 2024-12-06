using System.Diagnostics;

internal class AdventSolver
{
    private readonly IDatasetReader _datasetReader;

    public AdventSolver(IDatasetReader datasetReader)
    {
        _datasetReader = datasetReader;
    }

    public async Task Solve(int year, int day, string? sample = null)
    {
        try
        {
            var data = await _datasetReader.ReadDatasetAsync(year, day, sample);
            var dayImpl = GetDay(day, data);
            var watch = Stopwatch.StartNew();
            var part1 = await dayImpl.Part1(data);
            watch.Stop();
            var ms1 = watch.ElapsedMilliseconds;
            watch.Restart();
            var part2 = await dayImpl.Part2(data);
            watch.Stop();
            var ms2 = watch.ElapsedMilliseconds;
            Console.WriteLine($"Day: {day}");
            Console.WriteLine($"=======");
            Console.WriteLine($"Part 1: {part1} ({ms1} milliseconds)");
            Console.WriteLine($"Part 2: {part2} ({ms2} milliseconds)");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }

    private IDay GetDay(int day, List<string> data)
    {
        return day switch
        {
            1 => new Day01(),
            2 => new Day02(data),
            3 => new Day03(),
            4 => new Day04(),
            5 => new Day05(data),
            6 => new Day06(data),
            7 => new Day07(),
            8 => new Day08(),
            9 => new Day09(),
            10 => new Day10(),
            11 => new Day11(),
            12 => new Day12(),
            13 => new Day13(),
            14 => new Day14(),
            15 => new Day15(),
            16 => new Day16(),
            17 => new Day17(),
            18 => new Day18(),
            19 => new Day19(),
            20 => new Day20(),
            21 => new Day21(),
            22 => new Day22(),
            23 => new Day23(),
            24 => new Day24(),
            25 => new Day25(),
            _ => throw new ArgumentOutOfRangeException(nameof(day), $"Day {day} is not implemented yet.")
        };
    }
}
