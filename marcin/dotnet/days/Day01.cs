namespace AoC2024.days
{
    internal class Day01: IDay
    {
        public async Task<int> Part1(List<string> data)
        {
            int result = 0;
            var lists = ParseList(data);
            lists.left.Sort();
            lists.right.Sort();
            for (int i = 0; i < lists.left.Count; i++)
            {
                result += Math.Abs(lists.left[i] - lists.right[i]);
            }
            return await Task.FromResult(result);
        }

        public async Task<int> Part2(List<string> data)
        {
            int result = 0;

            Dictionary<int, int> occurences = new Dictionary<int, int>();
            var lists = ParseList(data);

            foreach (var l in lists.left)
            {
                if (occurences.ContainsKey(l))
                {
                    result += (l * occurences[l]);
                }
                else
                {
                    var c = lists.right.Where(r => r == l).Count();
                    occurences.Add(l, c);
                    result += (l * c);
                }
            }

            return await Task.FromResult(result);
        }

        private (List<int> left, List<int> right) ParseList(List<string> data)
        {
            var left = new List<int>();
            var right = new List<int>();
            foreach (var line in data)
            {
                var parts = line.Split("   ");
                left.Add(int.Parse(parts[0]));
                right.Add(int.Parse(parts[1]));
            }
            return (left, right);
        }

    }
}
