using Utilities;
namespace AdventOfCode;

public class Day01 : BaseDay
{
    private readonly List<string> _input;
    private List<int> firstpositions = new List<int>();
    private List<int> lastpositions = new List<int>();
    private Dictionary<int, int> occurenceList = new Dictionary<int, int>();
    public Day01()
    {
        _input = File.ReadAllText(InputFilePath).SplitByNewline();
    }

    private void GenerateCollection(List<string> input, bool isPartTwo = false)
    {
        firstpositions = new List<int>();
        lastpositions = new List<int>();
        foreach (var line in input)
        {
            var segment = line.Split("   ");
            var converted = Convert.ToInt32(segment[1].Trim());
            firstpositions.Add(Convert.ToInt32(segment[0].Trim()));
            
            if (isPartTwo)
            {
                if (occurenceList.ContainsKey(converted))
                    occurenceList[converted] += 1;
                else occurenceList.Add(converted, 1);
            }
            else
            {
                lastpositions.Add(converted);
            }
        }
    }

    private string ProcessInput1(List<string> input)
    {
        GenerateCollection(input);
        int sum = 0;

        firstpositions.Sort();
        lastpositions.Sort();

        var len = firstpositions.Count;
        for (var i = 0; i < len; i++)
        {
            sum += Math.Abs(firstpositions[i] - lastpositions[i]);
        }
        
        return $"{sum}";
    }

    private string ProcessInput2(List<string> input)
    {
        GenerateCollection(input, true);
        int sum = 0;

        var len = firstpositions.Count;
        for (var i = 0; i < len; i++)
        {
            var appearances = occurenceList.GetValueOrDefault(firstpositions[i]);
            if (appearances == default) continue;
            sum += firstpositions[i] * appearances;
        }

        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
