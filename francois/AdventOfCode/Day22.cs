using Utilities;

namespace AdventOfCode;

public class Day22 : BaseDay
{
    private readonly string _input;
    private List<long> secrets = new List<long>();
    private Dictionary<int, int> sequences = new Dictionary<int, int>();
    private readonly long pruneval = 0xFFFFFF; // 16777216-1
    private long max = 0;

    public Day22()
    {
        _input = File.ReadAllText(InputFilePath);
        secrets = _input.ToLongList("\r\n");
    }

    private string ProcessInput1()
    {
        long sum = 0;
        HashSet<int> seen = new HashSet<int>();
        foreach (var key in secrets)
        {
            seen.Clear();
            int change = 0;
            var init = key;
            int prev = 0;
            for (int i = 0; i < 2000; i++)
            {
                init = ((init << 6) ^ init) & pruneval;
                init = ((init >> 5) ^ init) & pruneval;
                init = ((init << 11) ^ init) & pruneval;

                var num = (int)(init % 10);
                change = (change << 8) | (byte)(num - prev);
                prev = num;

                if(i > 2 && 
                   seen.Add(change) && 
                   !sequences.TryAdd(change, prev))
                {
                    sequences[change] += prev;
                    if (sequences[change] > max) max = sequences[change];
                }
            }
            sum += init;
        }
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        return $"{max}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
