using Utilities;
namespace AdventOfCode;

public class Day02 : BaseDay
{
    private readonly List<string> _input;
    public Day02()
    {
        _input = File.ReadAllText(InputFilePath).SplitByNewline();
    }
    private string ProcessInput1(List<string> input)
    {
        int sum = 0;

        foreach (var line in input)
        {
            var segment = line.Split(" ");
            var levellist = new List<int>();
            bool isAsc = false;
            bool isDesc = false;
            for(var i = 0; i < segment.Length; i++)
            {
                var current = Convert.ToInt32(segment[i]);
                if (i == segment.Length - 1)
                {
                    sum++;
                    break;
                }
                var next = Convert.ToInt32(segment[i + 1]);

                if (current > next)
                {
                    if (!isAsc && !isDesc) isAsc = true;

                    if (isDesc) break;
                }
                else if (current < next)
                {
                    if (!isAsc && !isDesc) isDesc = true;
                    if (isAsc) break;
                }
                var diff = Math.Abs(current - next);
                if (diff > 3 || diff < 1) break;
            }
        }

        return $"{sum}";
    }

    private string ProcessInput2(List<string> input)
    {
        int sum = 0;

        foreach (var line in input)
        {
            var segment = line.Split(" ");
            var levellist = new List<int>();
            bool isAsc = false;
            bool isDesc = false;
            bool hasDamp = false;
            int dampind = 0;
            for (var i = 0; i < segment.Length; i++)
            {
                var current = Convert.ToInt32(segment[i]);
                if (i == segment.Length - 1)
                {
                    sum++;
                    break;
                }
                var next = Convert.ToInt32(segment[i + 1]);

                if (current > next)
                {
                    if (!isAsc && !isDesc) isAsc = true;

                    if (isDesc)
                    {
                        hasDamp = true;
                        dampind = i;
                        break;
                    }
                }
                else if (current < next)
                {
                    if (!isAsc && !isDesc) isDesc = true;
                    if (isAsc) 
                    {
                        hasDamp = true;
                        dampind = i;
                        break;
                    }
                }
                var diff = Math.Abs(current - next);
                if (diff > 3 || diff < 1)
                {
                    hasDamp = true;
                    dampind = i;
                    break;
                }
            }

            if (hasDamp)
            {
                for(var x = 0; x < segment.Length; x++)
                {
                    isAsc = false;
                    isDesc = false;
                    bool canSatisfy = false;
                    var newList = new List<string>(segment);
                    newList.RemoveAt(x);

                    for (var i = 0; i < newList.Count; i++)
                    {
                        var current = Convert.ToInt32(newList[i]);
                        if (i == newList.Count - 1)
                        {
                            canSatisfy = true;
                            break;
                        }
                        var next = Convert.ToInt32(newList[i + 1]);

                        if (current > next)
                        {
                            if (!isAsc && !isDesc) isAsc = true;

                            if (isDesc)
                            {
                                break;
                            }
                        }
                        else if (current < next)
                        {
                            if (!isAsc && !isDesc) isDesc = true;
                            if (isAsc)
                            {
                                break;
                            }
                        }
                        var diff = Math.Abs(current - next);
                        if (diff > 3 || diff < 1)
                        {
                            break;
                        }
                    }
                    if (canSatisfy)
                    {
                        sum++;
                        break;
                    }
                }
            }

        }

        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
