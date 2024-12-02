internal class Day02: IDay
{
    public async Task<int> Part1(List<string> data)
    {
        var reports = data.Select(x => x.Split(" ").Select(x => int.Parse(x)).ToList()).ToList();
        return await Task.FromResult(reports.Where(FilterLevels).Count());
    }

    public async Task<int> Part2(List<string> data)
    {
        var reports = data.Select(x => x.Split(" ").Select(x => int.Parse(x)).ToList()).ToList();
        var safeReports = reports.Where(levels =>
        {
            var isSafe = FilterLevels(levels);
            return !isSafe ?
                levels.Select((_, idx) => idx)
                    .Aggregate(false, (dampened, lvlIdx) => !dampened ? FilterLevels(levels.Where((_, idx) => idx != lvlIdx).ToList()) : true)
                : isSafe;
        });
        return await Task.FromResult(safeReports.Count());
    }

    private bool FilterLevels(List<int> levels)
    {
        var decending = true;
        var ascending = true;
        var inBounds = true;
        for (int i = 0; i < levels.Count - 1; i++)
        {
            var (current, next) = (levels[i], levels[i + 1]);
            var diff = Math.Abs(next - current);
            if (current >= next) decending = false;
            if (current <= next) ascending = false;
            if (diff > 3) inBounds = false;
        }
        return inBounds && (decending || ascending);
    }
}