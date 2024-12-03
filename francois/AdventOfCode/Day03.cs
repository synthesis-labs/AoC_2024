using System.Text.RegularExpressions;
namespace AdventOfCode;

public class Day03 : BaseDay
{
    private readonly string _input;
    private readonly static Regex mulReg = new Regex(@"(mul\(\d+,\d+\))");
    private readonly static Regex commands = new Regex(@"(mul\(\d+,\d+\)|do\(\)|don\'t\(\))");

    public Day03()
    {
        _input = File.ReadAllText(InputFilePath);
    }
    private string ProcessInput1(string input)
    {
        int sum = 0;
        var matches = mulReg.Matches(input);
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
        var commandOrder = commands.Matches(input);
        Match previousCommand = null;
        for(var i = 0; i < commandOrder.Count; i++)
        {
            var pattern = commandOrder[i].Value;
            bool canCompute = true;
            bool isMul = false;
            if(previousCommand != null && previousCommand.Value == "don't()") canCompute = false;

            if (!pattern.Contains("mul(")) previousCommand = commandOrder[i];
            else isMul = true;

            if (canCompute && isMul)
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
