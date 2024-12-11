internal class Day02: IDay
{
    private readonly List<List<int>> _reports;
    public Day02(List<string> data) => _reports = data.Select(x => x.Split(" ").Select(x => int.Parse(x)).ToList()).ToList();

    public double Part1(List<string> data) => _reports.Count(FilterLevels);

    public double Part2(List<string> data) => _reports.Count(levels =>  FilterLevels(levels) || levels.Select((_, idx) => idx).Aggregate(false, (dampened, lvlIdx) => dampened || FilterLevels(levels.Where((_, idx) => idx != lvlIdx).ToList())));

    private bool FilterLevels(List<int> levels)
    {
        var (desc, asc, limit) = levels.SkipLast(1).Select((value, index) => (index, value)).Aggregate((true, true, true), (acc, current) =>
        {
            var (desc, asc, limit) = acc;
            var next = levels[current.index + 1];
            return(desc && current.value < next, asc && current.value > next, limit && Math.Abs(next - current.value) <= 3);
        });
        return limit && (asc || desc);
    }
}