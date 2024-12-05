internal class Day05 : IDay
{
    private readonly Tuple<int, int> _sums;

    public Day05(List<string> data)
    {
        var rules = data.Where(d => d.Contains("|")).ToDictionary(d => d, d => "");
        var updates = data.Where(d => d.Contains(",")).Select(d => d.Split(',').Select(d => int.Parse(d)).ToList());
        var (validSum, sortedInvalidSum) = (0, 0);
        foreach (var update in updates)
        {
            var (isValid, isSorted) = (true, true);
            do
            {
                isSorted = true;
                for (int i = 0; i < update.Count; i++)
                    for (int j = i; j < update.Count; j++)
                    {
                        if (i != j && rules.ContainsKey($"{update[j]}|{update[i]}"))
                        {
                            isValid = isSorted = false;
                            int swap = update[j];
                            update[j] = update[i];
                            update[i] = swap;
                        }
                    }
            } while (!isSorted);
            if (isValid) validSum += update[(int)Math.Ceiling((double)update.Count / 2) - 1];
            else sortedInvalidSum += update[(int)Math.Ceiling((double)update.Count / 2) - 1];
        }
        _sums = new Tuple<int, int>(validSum, sortedInvalidSum);
    }
    
    public async Task<int> Part1(List<string> data) => _sums.Item1;

    public async Task<int> Part2(List<string> data) => _sums.Item2;
}