using AoC2024.days;

namespace AoC2024
{
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
                var dayImpl = GetDay(day);
                var part1 = await dayImpl.Part1(data);
                var part2 = await dayImpl.Part2(data);
                Console.WriteLine($"Day: {day}");
                Console.WriteLine($"=======");
                Console.WriteLine($"Part 1: {part1}");
                Console.WriteLine($"Part 2: {part2}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }

        private IDay GetDay(int day)
        {
            return day switch
            {
                1 => new Day01(),
                2 => new Day02(),
                3 => new Day03(),
                4 => new Day04(),
                5 => new Day05(),
                6 => new Day06(),
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
}
