internal class Day01: IDay
{
    public double Part1(List<string> data)
    {
        var (first, second) = ParseList(data);
        first.Sort();
        second.Sort();
        return first.Select((f, i) => Math.Abs(f - second[i])).Sum();
    }

    public double Part2(List<string> data)
    {
        var occurences = new Dictionary<int, int>();
        var (first, second) = ParseList(data);
        return first.Aggregate(0, (sum, l) =>
        {
            if (!occurences.ContainsKey(l))
                occurences.Add(l, second.Where(r => r == l).Count());
            sum += (l * occurences[l]);
            return sum;
        });
    }

    private (List<int> first, List<int> second) ParseList(List<string> data)
    {
        return data.Aggregate((new List<int>(), new List<int>()), (acc, line) =>
        {
            var parts = line.Split("   ");
            acc.Item1.Add(int.Parse(parts[0]));
            acc.Item2.Add(int.Parse(parts[1]));
            return acc;
        });
    }
}