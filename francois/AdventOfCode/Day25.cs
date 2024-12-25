using Utilities;

namespace AdventOfCode;

public class Day25 : BaseDay
{
    private readonly string _input;
    private List<List<int>> keys = new List<List<int>>();
    private List<List<int>> locks = new List<List<int>>();

    public Day25()
    {
        _input = File.ReadAllText(InputFilePath);

        var splits = _input.SplitByDoubleNewline();
        foreach(var split in splits)
        {
            var keynum = new List<int>();
            var keysplit = split.SplitIntoColumns();
            if (split.StartsWith('#'))
            {
                foreach(var key in keysplit)
                {
                    var col = key.Count(q => q == '#') - 1;
                    keynum.Add(col);
                }
                locks.Add(keynum);
            }
            else
            {
                foreach (var key in keysplit)
                {
                    var col = key.Count(q => q == '#') - 1;
                    keynum.Add(col);
                }
                keys.Add(keynum);
            }
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        foreach(var loc in locks)
        {
            foreach(var key in keys)
            {
                var match = true;
                for(var i = 0; i < key.Count(); i++)
                {
                    var tot = loc[i] + key[i];
                    if(tot > 5)
                    {
                        match = false;
                        break;
                    }
                }

                if(match)
                {
                    sum++;
                }
            }
        }
        return $"{sum}";
    }
    private string ProcessInput2()
    {
        long sum = 50;
        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
