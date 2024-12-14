using System.Text.RegularExpressions;

internal class Day03 : IDay
{
    public double Part1(List<string> data) => Regex.Matches(string.Join("", data), @"mul\((\d{1,3}),(\d{1,3})\)").Aggregate(0, (sum, match) => sum += int.Parse(match.Groups[1].Value) * int.Parse(match.Groups[2].Value));
    public double Part2(List<string> data) => Regex.Matches(Regex.Replace(string.Join("", data), @"don't\(\).*?(do\(\)|$)", ""), @"mul\((\d{1,3}),(\d{1,3})\)").Aggregate(0, (sum, match) => sum += int.Parse(match.Groups[1].Value) * int.Parse(match.Groups[2].Value));
}