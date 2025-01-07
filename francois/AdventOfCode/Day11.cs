using Utilities;

namespace AdventOfCode;

public class Day11 : BaseDay
{
    ExtendedDictionary<long, long> cacheList = new ExtendedDictionary<long, long>();
    private readonly string _input;
    public Day11()
    {
        _input = File.ReadAllText(InputFilePath);
        foreach(var stone in _input.ToLongList(" "))
        {
            if (!cacheList.TryAdd(stone, 1))
            {
                cacheList[stone]++;
            }
        }
    }
    private string ProcessInput1(string input)
    {
        long sum = 0;
        for(var x = 0; x < 25; x++)
        {
            ExtendedDictionary<long, long> newList = new ExtendedDictionary<long, long>();
            foreach (var stone in cacheList)
            {
                var stoneString = stone.Key.ToString();
                if (stone.Key == 0)
                {
                    newList[1] += stone.Value;
                }
                else if (stoneString.Length % 2 == 0)
                {
                    var stringval = stoneString.Split(stoneString.Length >> 1).ToList();
                    var newSplit = stringval.Select(q => string.Join("", q)).ToList();
                    var parse1 = long.Parse(newSplit[0]);
                    var parse2 = long.Parse(newSplit[1]);

                    newList[parse1] += stone.Value;
                    newList[parse2] += stone.Value;
                }
                else
                {
                    var newKey = stone.Key * 2024;
                    newList[newKey] += stone.Value;
                }
            }
            cacheList = newList;
        }
        sum = cacheList.Sum(q => q.Value);
        return $"{sum}";
    }

    private string ProcessInput2(string input)
    {
        long sum = 0;
        for (var x = 0; x < 50; x++)
        {
            ExtendedDictionary<long, long> newList = new ExtendedDictionary<long, long>();
            foreach (var stone in cacheList)
            {
                var stoneString = stone.Key.ToString();
                if (stone.Key == 0)
                {
                    newList[1] += stone.Value;
                }
                else if (stoneString.Length % 2 == 0)
                {
                    var stringval = stoneString.Split(stoneString.Length >> 1).ToList();
                    var newSplit = stringval.Select(q => string.Join("", q)).ToList();
                    var parse1 = long.Parse(newSplit[0]);
                    var parse2 = long.Parse(newSplit[1]);

                    newList[parse1] += stone.Value;
                    newList[parse2] += stone.Value;
                }
                else
                {
                    var newKey = stone.Key * 2024;
                    newList[newKey] += stone.Value;
                }
            }
            cacheList = newList;
        }
        sum = cacheList.Sum(q => q.Value);
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
