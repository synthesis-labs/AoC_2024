internal class Day11 : IDay
{
    public double Part1(List<string> data) => CountStones(data, 25);
    public double Part2(List<string> data) => CountStones(data, 75);

    private void Change(Dictionary<string, double> seen, string[] keys, double count)
    {
        foreach (var key in keys) 
            if (seen.ContainsKey(key)) seen[key] += count;
            else seen.Add(key, count);
    }

    private double CountStones(List<string> data, int blinks)
    {
        var stones = data.SelectMany(d => d.Split(' ')).ToDictionary(k => k, v => 1d);
        for (int blink = 0; blink < blinks; blink++)
        {
            var seen = new Dictionary<string, double>();
            foreach (var key in stones.Keys)
            {
                int half = key.Length / 2;
                var (left, right, multi) = ($"{double.Parse(key.Substring(0, Math.Max(half, 1)))}", $"{double.Parse(key.Substring(half))}", $"{double.Parse(key) * 2024}");
                Change(seen, key == "0" ? ["1"] : (key.Length % 2 == 0 ? [left, right] : [multi]), stones[key]);
            }
            stones = seen;
        }
        return stones.Aggregate(0d, (total, item) => total += item.Value);
    }
}