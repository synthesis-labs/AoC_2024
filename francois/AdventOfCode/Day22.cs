using System.Collections.Concurrent;

namespace AdventOfCode;

public class Day22 : BaseDay
{
    private readonly string[] _input;
    private ConcurrentBag<(long, List<long>)> secrets = new ConcurrentBag<(long, List<long>)>();
    private ConcurrentDictionary<string, long> sequences = new ConcurrentDictionary<string, long>();
    private readonly long pruneval = 16777216;

    public Day22()
    {
        _input = File.ReadAllLines(InputFilePath);
        foreach(var line in _input)
        {
            secrets.Add((long.Parse(line), new List<long>()));
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

        Parallel.ForEach(secrets, (key) =>
        {
            var seen = new HashSet<string>();
            var init = key.Item1;
            var sec = key.Item1;
            key.Item2.Add(sec % 10);
            for (int i = 0; i < 2000; i++)
            {
                init = Step1(sec);
                sec = Prune(Mix(init, sec));
                init = Step2(sec);
                sec = Prune(Mix(init, sec));
                init = Step3(sec);
                sec = Prune(Mix(init, sec));
                key.Item2.Add(sec % 10);

                if (key.Item2.Count >= 5)
                {
                    var l1 = key.Item2.TakeLast(5);
                    var l2 = key.Item2.TakeLast(4);
                    var check = l1.Zip(l2, (a, b) => b - a);
                    var seq = string.Join(',', check);
                    if (!seen.Contains(seq))
                    {
                        if (!sequences.TryAdd(seq, key.Item2.Last()))
                        {
                            sequences[seq] += key.Item2.Last();
                        }

                        seen.Add(seq);
                    }
                }
            }
            sum += sec;
        });
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
