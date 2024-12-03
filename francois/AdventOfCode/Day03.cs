using System.Text.RegularExpressions;
namespace AdventOfCode;

public class Day03 : BaseDay
{
    private readonly string _input;
    private Regex mulReg = new Regex("(mul\\(\\d+,\\d+\\))");
    private Regex commands = new Regex("(do\\(\\)|don\\'t\\(\\))");

    public Day03()
    {
        _input = File.ReadAllText(InputFilePath);
    }
    private string ProcessInput1(string input)
    {
        int sum = 0;
        var matches = mulReg.Matches(input).ToArray();
        foreach (var match in matches)
        {
            var pattern = match.ToString();
            var split = pattern.Split(',');
            var val1 = int.Parse(split[0].Replace("mul(", ""));
            var val2 = int.Parse(split[1].Trim(')'));
            sum += val1 * val2;
        }
        return $"{sum}";
    }

    private string ProcessInput2(string input)
    {
        int sum = 0;
        var matches = mulReg.Matches(input).ToArray();
        var commandOrder = commands.Matches(input).ToArray();
        foreach (var match in matches)
        {
            var pattern = match.ToString();
            var canCompute = true;
            if(match.Index > commandOrder.FirstOrDefault()?.Index)
            {
                var lastCommand = commandOrder.Last(q => q.Index < match.Index);
                if(lastCommand.Value == "don't()") canCompute = false;
            }

            if(canCompute)
            {
                var split = pattern.Split(',');
                var val1 = int.Parse(split[0].Replace("mul(", ""));
                var val2 = int.Parse(split[1].Trim(')'));
                sum += val1 * val2;
            }
        }
        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
