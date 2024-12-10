internal class Day10: IDay
{
    public async Task<int> Part1(List<string> data) => HikeTrails(data).score;
    public async Task<int> Part2(List<string> data) => HikeTrails(data).rating;

    private (int score, int rating) HikeTrails(List<string> data)
    {
        var trails = data.SelectMany((d, r) => d.Select((d, c) => new Path { Elavation = int.Parse($"{d}"), Position = (r, c) })).ToList();
        trails.ForEach(path =>
        {
            var (r, c) = path.Position;
            var links = trails.Where(t => t.Elavation == path.Elavation + 1 && (t.Position == (r - 1, c) || t.Position == (r + 1, c) || t.Position == (r, c - 1) || t.Position == (r, c + 1)));
            path.Links.AddRange(links);
        });

        return trails.Where(t => t.Elavation == 0).Aggregate((0, 0), (metrics, path) =>
        {
            var (score, rating) = metrics;
            var ends = new List<Path>();
            Step(path, ends);
            score += ends.Distinct().Count();
            rating += ends.Count();
            return (score, rating);
        });
    }

    private void Step(Path path, List<Path> ends)
    {
        if (path.Elavation == 9) ends.Add(path);
        else path.Links.ForEach(link => Step(link, ends));
    }

    private class Path
    {
        public int Elavation { get; set; }
        public (int r, int c) Position { get; set; }
        public List<Path> Links { get; set; } = new List<Path>();
    }
}