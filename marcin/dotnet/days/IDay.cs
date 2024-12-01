namespace AoC2024.days
{
    internal interface IDay
    {
        Task<int> Part1(List<string> data);
        Task<int> Part2(List<string> data);
    }
}
