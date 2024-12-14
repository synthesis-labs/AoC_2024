using Utilities;

namespace AdventOfCode;

public class Day11 : BaseDay
{
    //private List<List<string>> StoneList = new List<List<string>>();
    //private List<List<string>> FinalStones = new List<List<string>>();
    Dictionary<long, long> cacheList = new Dictionary<long, long>();
    private readonly string _input;
    public Day11()
    {
        _input = File.ReadAllText(InputFilePath);
        foreach(var stone in _input.ExtractLongs().ToList())
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
            Dictionary<long, long> newList = new Dictionary<long, long>();
            foreach(var stone in cacheList)
            {
                var stoneString = stone.Key.ToString();
                if (stone.Key == 0)
                {
                    if (!newList.TryAdd(1, stone.Value))
                    {
                        newList[1] += stone.Value;
                    }
                }
                else if (stoneString.Length % 2 == 0)
                {
                    var stringval = stoneString.Split(stoneString.Length / 2).ToList();
                    var newSplit = stringval.Select(q => string.Join("", q)).ToList();
                    var parse1 = long.Parse(newSplit[0]);
                    var parse2 = long.Parse(newSplit[1]);
                    
                    if (!newList.TryAdd(parse1, stone.Value))
                    {
                        newList[parse1] += stone.Value;
                    }
                    
                    if (!newList.TryAdd(parse2, stone.Value))
                    {
                        newList[parse2] += stone.Value;
                    }
                }
                else
                {
                    var newKey = stone.Key * 2024;
                    if (!newList.TryAdd(newKey, stone.Value))
                    {
                        newList[newKey] += stone.Value;
                    }
                }
            }
            cacheList = newList;
        }
        #region Original Brute Force Part 1
        //for (int x = 0; x < 25; x++)
        //{
        //    //FinalStones = new List<List<string>>();
        //    //for (int y = 0; y < StoneList.LongCount(); y++)
        //    //{
        //    //    var stones = StoneList[y];
        //    //    var newList = new List<string>();
        //    //    foreach (var stone in stones)
        //    //    {
        //    //        if (stone == "0")
        //    //        {
        //    //            newList.Add("1");
        //    //        }
        //    //        else if (stone.Length % 2 == 0)
        //    //        {
        //    //            var splits = stone.Split(stone.Length / 2);
        //    //            var newSplit = splits.Select(q => string.Join("", q)).ToList();
        //    //            var first = long.Parse(newSplit[0]);
        //    //            var second = long.Parse(newSplit[1]);
        //    //            newList.Add(first.ToString());
        //    //            newList.Add(second.ToString());
        //    //        }
        //    //        else
        //    //        {
        //    //            var newStone = long.Parse(stone) * 2024;
        //    //            newList.Add(newStone.ToString());
        //    //        }
        //    //    }
        //    //    FinalStones.Add(newList);
        //    //}
        //    //var newStonelist = FinalStones.SelectMany(stone => stone).ToList();
        //    //StoneList = new List<List<string>>(new List<List<string>>() { newStonelist });
        //}
        //sum = StoneList.Sum(q => q.Sum());
        #endregion Original Brute Force Part 1
        sum = cacheList.Sum(q => q.Value);
        return $"{sum}";
    }

    private string ProcessInput2(string input)
    {
        long sum = 0;
        for (var x = 0; x < 50; x++)
        {
            Dictionary<long, long> newList = new Dictionary<long, long>();
            foreach (var stone in cacheList)
            {
                var stoneString = stone.Key.ToString();
                if (stone.Key == 0)
                {
                    if (!newList.TryAdd(1, stone.Value))
                    {
                        newList[1] += stone.Value;
                    }
                }
                else if (stoneString.Length % 2 == 0)
                {
                    var stringval = stoneString.Split(stoneString.Length / 2).ToList();
                    var newSplit = stringval.Select(q => string.Join("", q)).ToList();
                    var parse1 = long.Parse(newSplit[0]);
                    var parse2 = long.Parse(newSplit[1]);

                    if (!newList.TryAdd(parse1, stone.Value))
                    {
                        newList[parse1] += stone.Value;
                    }

                    if (!newList.TryAdd(parse2, stone.Value))
                    {
                        newList[parse2] += stone.Value;
                    }
                }
                else
                {
                    var newKey = stone.Key * 2024;
                    if (!newList.TryAdd(newKey, stone.Value))
                    {
                        newList[newKey] += stone.Value;
                    }
                }
            }
            cacheList = newList;
        }
        #region Original Brute Force Part 2
        //for (int x = 0; x < 50; x++)
        //{
        //    //FinalStones = new List<List<string>>();
        //    //for (int y = 0; y < StoneList.LongCount(); y++)
        //    //{
        //    //    var stones = StoneList[y];
        //    //    var newList = new List<string>();
        //    //    foreach (var stone in stones)
        //    //    {
        //    //        if (stone == "0")
        //    //        {
        //    //            newList.Add("1");
        //    //        }
        //    //        else if (stone.Length % 2 == 0)
        //    //        {
        //    //            var splits = stone.Split(stone.Length / 2);
        //    //            var newSplit = splits.Select(q => string.Join("", q)).ToList();
        //    //            var first = long.Parse(newSplit[0]);
        //    //            var second = long.Parse(newSplit[1]);
        //    //            newList.Add(first.ToString());
        //    //            newList.Add(second.ToString());
        //    //        }
        //    //        else
        //    //        {
        //    //            var newStone = long.Parse(stone) * 2024;
        //    //            newList.Add(newStone.ToString());
        //    //        }
        //    //    }
        //    //    FinalStones.Add(newList);
        //    //}
        //    //var newStonelist = FinalStones.SelectMany(stone => stone).ToList();
        //    //StoneList = new List<List<string>>(new List<List<string>>() { newStonelist });
        //}
        //sum = StoneList.Sum(q => q.Sum());
        #endregion Original Brute Force Part 2
        sum = cacheList.Sum(q => q.Value);
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
