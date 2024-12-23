namespace AdventOfCode;

public class Day22 : BaseDay
{
    private readonly string[] _input;
    private List<(long, List<long>, List<long>)> secrets = new List<(long, List<long>, List<long>)>();
    private Dictionary<string, long> sequences = new Dictionary<string, long>();
    private readonly long pruneval = 16777216;

    public Day22()
    {
        _input = File.ReadAllLines(InputFilePath);
        foreach(var line in _input)
        {
            secrets.Add((long.Parse(line), new List<long>(), new List<long>()));
        }
    }

    private long Step3(long cur)
    {
        return cur * 2048;
    }

    private long Step2(decimal cur)
    {
        return Convert.ToInt64(Math.Floor(cur/32));
    }

    private long Step1(long cur)
    {
        return cur * 64;
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
        foreach(var key in secrets)
        {
            var seen = new List<string>();
            var init = key.Item1;
            var sec = key.Item1;
            key.Item3.Add(sec % 10);
            for (int i = 0; i < 2000; i++)
            {
                init = Step1(sec);
                sec = Prune(Mix(init, sec));
                init = Step2(sec);
                sec = Prune(Mix(init, sec));
                init = Step3(sec);
                sec = Prune(Mix(init, sec));
                key.Item2.Add(sec);
                key.Item3.Add(sec % 10);

                if(key.Item3.Count >= 5)
                {
                    var check = key.Item3.Skip(key.Item3.Count - 5).Zip(key.Item3.Skip(key.Item3.Count - 4), (a, b) => b - a);
                    var seq = string.Join(",", check);
                    if (!seen.Contains(seq))
                    {
                        if (!sequences.TryAdd(seq, key.Item3.Last()))
                        {
                            sequences[seq] += key.Item3.Last();
                        }
                        seen.Add(seq);
                    }
                }
            }
        }

        sum = secrets.Sum(q => q.Item2.Last());
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
