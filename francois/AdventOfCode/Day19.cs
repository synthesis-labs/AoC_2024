namespace AdventOfCode;

public class Day19 : BaseDay
{
    private readonly string[] _input;
    private HashSet<string> AvailableTowels = new HashSet<string>();
    private HashSet<string> Combos = new HashSet<string>();
    private Dictionary<string, long> TowelCombos = new Dictionary<string, long>();

    public Day19()
    {
        _input = File.ReadAllLines(InputFilePath);

        foreach (var line in _input)
        {
            if (line.Contains(','))
            {
                var split = line.Split(',');
                foreach (var item in split)
                {
                    AvailableTowels.Add(item.Trim());
                }
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                Combos.Add(line);
            }
        }
    }

    public long GetPatterns(string pattern, HashSet<string> possible)
    {
        if (TowelCombos.TryGetValue(pattern, out long total)) return total;

        TowelCombos[pattern] = possible
                                .Where(q => pattern.StartsWith(q))
                                .Sum(p => GetPatterns(pattern[p.Length..], possible));
        return TowelCombos[pattern];
    }

    private string ProcessInput1()
    {
        long sum = 0;
        TowelCombos.Clear();
        TowelCombos.Add(string.Empty, 1);
        sum = Combos.Count(combo => GetPatterns(combo, AvailableTowels) > 0);
        return $"{sum}";
    }

    private string ProcessInput2()
    {
        long sum = 0;
        TowelCombos.Clear();
        TowelCombos.Add(string.Empty, 1);
        sum = Combos.Sum(combo => GetPatterns(combo, AvailableTowels));
        return $"{sum}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
