namespace AdventOfCode;

public class Day19 : BaseDay
{
    private readonly string[] _input;
    private HashSet<string> AvailableTowels = new HashSet<string>();
    private HashSet<string> Combos = new HashSet<string>();
    private long[] cache;

    public Day19()
    {
        _input = File.ReadAllLines(InputFilePath);

        foreach (var line in _input)
        {
            if (line.Contains(','))
            {
                AvailableTowels = new HashSet<string>(line.Split(", "));
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                Combos.Add(line);
            }
        }
        cache = new long[Combos.Max(q => q.Length) + 1];
    }

    public long GetPatterns(ReadOnlySpan<char> pattern)
    {
        cache[pattern.Length] = 1;
        for(int i = pattern.Length -1; i > -1; i--)
        {
            cache[i] = 0;
            foreach(string towel in AvailableTowels)
            {
                int towelsize = i + towel.Length;
                if (towelsize > pattern.Length) continue;
                if (pattern[i..towelsize].SequenceEqual(towel))
                    cache[i] += cache[i + towel.Length];
            }
        }
        return cache[0];
    }

    private string ProcessInput1()
    {
        long sum = 0;
        sum = Combos.Count(combo => GetPatterns(combo) > 0);
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        sum = Combos.Sum(combo => GetPatterns(combo));
        return $"{sum}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
