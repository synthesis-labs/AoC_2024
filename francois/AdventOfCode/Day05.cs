using Utilities;

namespace AdventOfCode;

public class Day05 : BaseDay
{
    private readonly string[] _input;
    private List<(int, int)> order = new List<(int, int)>();
    private List<List<int>> inputs = new List<List<int>>();
    private List<List<int>> invalids = new List<List<int>>();

    public Day05()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var i = 0; i < _input.Length; i++)
        {
            if (_input[i].Contains('|'))
            {
                var split = _input[i].Split('|');
                order.Add((int.Parse(split[0]), int.Parse(split[1])));
            }
            else if (_input[i].Contains(","))
            {
                var split = _input[i].ToIntList(",");
                inputs.Add(split);
            }
        }
    }
    private string ProcessInput1(string[] input)
    {
        int sum = 0;

        for(var x = 0; x < inputs.Count; x++)
        {
            var isValid = true;
            for(var y = 0; y < inputs[x].Count; y++)
            {
                var checkval = inputs[x][y];
                var hasChecks = order.FindAll(q => q.Item2 == checkval);
                foreach(var check in hasChecks)
                {
                    if (inputs[x].IndexOf(check.Item1) > y)
                    {
                        isValid = false;
                        invalids.Add(inputs[x]);
                        break;
                    }
                }
                if (!isValid) break;
            }
            if(isValid)
            {
                var middle = Math.Ceiling(Convert.ToDecimal(inputs[x].Count / 2));
                sum += inputs[x][Convert.ToInt32(middle)];
            }
        }
        return $"{sum}";
    }
    private string ProcessInput2(string[] input)
    {
        if(invalids.Count == 0)
        {
            ProcessInput1(input);
        }
        int sum = 0;

        for (var x = 0; x < invalids.Count; x++)
        {
            for (int i = 0; i < invalids[x].Count - 1; i++)
                for (int j = 0; j < invalids[x].Count - i - 1; j++)
                {
                    var checkval = invalids[x][j];
                    var hasChecks = order.FindAll(q => q.Item2 == checkval);
                    foreach (var check in hasChecks)
                    {
                        var checkind = invalids[x].IndexOf(check.Item1);
                        if (checkind > j)
                        {
                            var newval = invalids[x][j];
                            invalids[x][j] = invalids[x][checkind];
                            invalids[x][checkind] = newval;
                        }
                    }
                }
            var middle = Math.Ceiling(Convert.ToDecimal(invalids[x].Count / 2));
            sum += invalids[x][Convert.ToInt32(middle)];
        }
        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
