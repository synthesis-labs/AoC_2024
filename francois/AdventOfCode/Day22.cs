using Utilities;

namespace AdventOfCode;

public class Day22 : BaseDay
{
    private readonly string _input;
    private List<int> secrets = new List<int>();
    private ExtendedDictionary<int, int> sequences = new ExtendedDictionary<int, int>();
    private readonly int pruneval = 0xFFFFFF; // 16777216-1
    private int max = 0;

    public Day22()
    {
        _input = File.ReadAllText(InputFilePath);
        secrets = _input.ToIntList("\r\n");
    }

    private string ProcessInput1()
    {
        long sum = 0;
        HashSet<int> seen = new HashSet<int>();
        for (int x = 0; x < secrets.Count; x++)
        {
            seen.Clear();
            int change = 0;
            var init = secrets[x];
            int prev = 0;
            int i = 0;
            do
            {
                init = ((init << 6) ^ init) & pruneval;
                init = ((init >> 5) ^ init) & pruneval;
                init = ((init << 11) ^ init) & pruneval;

                var num = (int)(init % 10);
                change = (change << 8) | (byte)(num - prev);
                prev = num;

                if (i > 2 &&
                   seen.Add(change))
                {
                    sequences[change] += prev;
                    if (sequences[change] > max) max = sequences[change];
                }
                i++;
            }
            while (i < 2000);
            
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
