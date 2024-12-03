using System.Text.RegularExpressions;

internal class Day03 : IDay
{
    private const string dontDo = @"don't\(\).*?(do\(\)|$)";
    private const string mul = @"mul\((\d{1,3}),(\d{1,3})\)";

    public async Task<int> Part1(List<string> data) => Regex.Matches(string.Join("", data), mul).Aggregate(0, (sum, match) => sum += int.Parse(match.Groups[1].Value) * int.Parse(match.Groups[2].Value));

    public async Task<int> Part2(List<string> data) => Regex.Matches(Regex.Replace(string.Join("", data), dontDo, ""), mul).Aggregate(0, (sum, match) => sum += int.Parse(match.Groups[1].Value) * int.Parse(match.Groups[2].Value));
}