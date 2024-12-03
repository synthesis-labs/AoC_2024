using System.Text.RegularExpressions;
namespace AdventOfCode;

public class Day03 : BaseDay
{
    private readonly string _input;
    public Day03()
    {
        _input = File.ReadAllText(InputFilePath);
    }
    private string ProcessInput1(string input)
    {
        int sum = 0;
        var end = false;
        var regex = new Regex("(mul\\(\\d+,\\d+\\))");
        var matches = regex.Matches(input);
        foreach (var match in matches)
        {
            var pattern = match.ToString();
            var split = pattern.Split(',');
            var val1 = int.Parse(split[0].Trim('m').Trim('u').Trim('l').Trim('('));
            var val2 = int.Parse(split[1].Trim(')'));
            sum += val1 * val2;
        }
        return $"{sum}";
    }

    private string ProcessInput2(string input)
    {
        int sum = 0;
        var end = false;
        var regex = new Regex("(mul\\(\\d+,\\d+\\))");
        var dos = new Regex("(do\\(\\))");
        var dont = new Regex("(don\\'t\\(\\))");
        var matches = regex.Matches(input).ToList();
        var doList = dos.Matches(input);
        var dontList = dont.Matches(input);
        var commandOrder = doList.Union(dontList).ToList().OrderBy(q => q.Index);
        foreach (var match in matches)
        {
            var pattern = match.ToString();
            var canCompute = true;
            if(match.Index > commandOrder.First().Index)
            {
                var lastCommand = commandOrder.Last(q => q.Index < match.Index);
                if(lastCommand.ToString() == "don't()")
                {
                    canCompute = false;
                }
                else
                {
                    canCompute = true;
                }
            }

            if(canCompute)
            {
                var split = pattern.Split(',');
                var val1 = int.Parse(split[0].Trim('m').Trim('u').Trim('l').Trim('('));
                var val2 = int.Parse(split[1].Trim(')'));
                sum += val1 * val2;
            }
        }
        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
