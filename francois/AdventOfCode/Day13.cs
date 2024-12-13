namespace AdventOfCode;

public class Day13 : BaseDay
{
    Dictionary<(long, long), List<(long, long)>> Prizes = new Dictionary<(long, long), List<(long, long)>>();
    //Dictionary<(long, long), long> Cost = new Dictionary<(long, long), long>();
    private readonly string[] _input;
    public Day13()
    {
        _input = File.ReadAllLines(InputFilePath);

        (long, long, List<(long, long)>) prize;
        prize.Item3 = new List<(long, long)>();
        foreach (var line in _input)
        {
            if (line.Contains("Prize:"))
            {
                var splits = line.Split(':');
                var nums = splits[1].Split(',');
                var x = long.Parse(nums[0].Trim().Substring(2));
                var y = long.Parse(nums[1].Trim().Substring(2));
                prize.Item1 = x;
                prize.Item2 = y;
                Prizes.Add((prize.Item1, prize.Item2), prize.Item3);
                prize.Item3 = new List<(long, long)>();
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                var splits = line.Split(':');
                var nums = splits[1].Split(',');
                var x = long.Parse(nums[0].Trim().Substring(2));
                var y = long.Parse(nums[1].Trim().Substring(2));
                prize.Item3.Add((x, y));
            }
        }
    }

    private string ProcessInput1(int max)
    {
        long sum = 0;
        foreach(var prize in Prizes)
        {
            var firstVal = prize.Value.First();
            var lastVal = prize.Value.Last();
            var prizex = prize.Key.Item1;
            var prizey = prize.Key.Item2;

            var check1 = (prizey * firstVal.Item1 - prizex * firstVal.Item2) / (lastVal.Item2 * firstVal.Item1 - lastVal.Item1 * firstVal.Item2);
            var check2 = (prizex - check1 * lastVal.Item1) / firstVal.Item1;

            var validx = check1 * lastVal.Item1 + check2 * firstVal.Item1 == prizex;
            var validy = check1 * lastVal.Item2 + check2 * firstVal.Item2 == prizey;
            if (check1 >= 0 && check2 >= 0 && validx && validy)
            {
                sum += (3 * check2 + check1);
            }
            //for (var x = max; x > 0; x--)
            //{
            //    if((firstVal.Item1 * x) == prize.Key.Item1 && 
            //       (firstVal.Item2 * x) == prize.Key.Item2)
            //    {
            //        var newCost = (x * 3);
            //        if (!Cost.TryAdd(prize.Key, newCost))
            //        {
            //            if (newCost < Cost[prize.Key])
            //                Cost[prize.Key] = newCost;
            //        }
            //    }
            //    for(var y = max; y > 0; y--)
            //    {
            //        if ((lastVal.Item1 * y) == prize.Key.Item1 &&
            //           (lastVal.Item2 * y) == prize.Key.Item2)
            //        {
            //            var newCost = (y * 1);
            //            if (!Cost.TryAdd(prize.Key, newCost))
            //            {
            //                if (newCost < Cost[prize.Key])
            //                    Cost[prize.Key] = newCost;
            //            }
            //        }
            //        if ((firstVal.Item1 * x) + (lastVal.Item1 * y) == prize.Key.Item1 &&
            //            (firstVal.Item2 * x) + (lastVal.Item2 * y) == prize.Key.Item2)
            //        {
            //            var newCost = (x * 3) + (y * 1);
            //            if(!Cost.TryAdd(prize.Key, newCost))
            //            {
            //                if(newCost < Cost[prize.Key])
            //                    Cost[prize.Key] = newCost;
            //            }
            //        }

            //        if ((firstVal.Item1 * x) + (lastVal.Item1 * y) < prize.Key.Item1 ||
            //           (firstVal.Item2 * x) + (lastVal.Item2 * y) < prize.Key.Item2)
            //        {
            //            break;
            //        }
            //    }
            //}
        }
        //sum = Cost.Sum(x => x.Value);
        return $"{sum}";
    }


    private string ProcessInput2()
    {
        long sum = 0;
        foreach (var prize in Prizes)
        {
            var firstVal = prize.Value.First();
            var lastVal = prize.Value.Last();
            var prizex = prize.Key.Item1 + 10000000000000;
            var prizey = prize.Key.Item2 + 10000000000000;

            var check1 = (prizey * firstVal.Item1 - prizex * firstVal.Item2) / (lastVal.Item2 * firstVal.Item1 - lastVal.Item1 * firstVal.Item2);
            var check2 = (prizex - check1 * lastVal.Item1) / firstVal.Item1;

            var validx = check1 * lastVal.Item1 + check2 * firstVal.Item1 == prizex;
            var validy = check1 * lastVal.Item2 + check2 * firstVal.Item2 == prizey;
            if (check1 >= 0 && check2 >= 0 && validx && validy)
            {
                sum += (3 * check2 + check1);
            }
        }
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(100));

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
