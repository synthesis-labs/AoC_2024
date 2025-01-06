using Utilities;

namespace AdventOfCode;

public class Day22 : BaseDay
{
    private readonly string _input;
    private Queue<long> secrets = new Queue<long>();
    private Dictionary<string, long> sequences = new Dictionary<string, long>();
    private readonly long pruneval = 16777216;

    public Day22()
    {
        _input = File.ReadAllText(InputFilePath);
        var list = _input.ToLongList("\r\n");
        foreach(var item in list)
        {
            secrets.Enqueue(item);
        }
    }

    private long Step3(long cur)
    {
        return cur << 11;
    }

    private long Step2(long cur)
    {
        return cur >> 5;
    }

    private long Step1(long cur)
    {
        return cur << 6;
    }

    private long Mix(long cur, long key)
    {
        return cur ^ key;
    }

    private long Prune(long key)
    {
        return key % pruneval;
    }

    private string ProcessInput1()
    {
        long sum = 0;
        while(secrets.TryDequeue(out var key))
        {
            string change = string.Empty;
            HashSet<string> seen = new HashSet<string>();
            var cur = key;
            var init = key;
            var sec = key;
            for (int i = 1; i <= 2000; i++)
            {
                init = Step1(sec);
                sec = Prune(Mix(init, sec));
                init = Step2(sec);
                sec = Prune(Mix(init, sec));
                init = Step3(sec);
                sec = Prune(Mix(init, sec));

                change += ((cur % 10) - (sec % 10));
                if (i > 4 && change[0] == '-') change = change[2..];
                else if (i > 4) change = change[1..];

                cur = sec;

                if(i >= 4 && seen.Add(change))
                {
                    if(!sequences.TryAdd(change, cur % 10))
                    {
                        sequences[change] += cur % 10;
                    }
                }
            }
            sum += sec;
        }
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        sum = sequences.Values.Max();
        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
