namespace AdventOfCode;

public class Day09 : BaseDay
{
    List<int> fullIterations = new List<int>();
    List<(int, int, int)> reserved = new List<(int, int, int)>(); // id, index, length
    List<(int, int)> freeSpace = new List<(int, int)>(); // index, length

    private readonly string _input;
    public Day09()
    {
        _input = File.ReadAllText(InputFilePath);

        for (var x = 1; x <= _input.Length; x += 2)
        {
            var len = Convert.ToInt32(_input[x - 1] - '0');
            var free = x == _input.Length ? 0 : Convert.ToInt32(_input[x] - '0');

            reserved.Prepend((x / 2, fullIterations.Count, len));
            freeSpace.Add((fullIterations.Count + len, free));

            fullIterations.AddRange(Enumerable.Repeat(x / 2, len));
            fullIterations.AddRange(Enumerable.Repeat(-1, free));
        }
    }
    private string ProcessInput1()
    {
        var fileList = new List<int>(fullIterations);
        long sum = 0;

        for(var x = fileList.Count - 1; x > 0; x--)
        {
            var cur = fileList[x];

            if (cur == -1) continue;

            var free = fileList.IndexOf(-1);

            if(free >= x) break;

            fileList[free] = cur;
            fileList[x] = -1;
        }

        for(var x = 0; x < fileList.Count; x++)
        {
            if (fileList[x] == -1) break;
            sum += (x * fileList[x]);
        }

        return $"{sum}";
    }

    private string ProcessInput2()
    {
        var fileList = new List<int>(fullIterations);
        long sum = 0;

        foreach(var taken in reserved)
        {
            var free = freeSpace.FirstOrDefault(q => q.Item1 < taken.Item2 && q.Item2 >= taken.Item3);

            if (free == default) continue;

            for(var x = 0; x < taken.Item3; x++)
            {
                fileList[free.Item1 + x] = taken.Item1;
                fileList[taken.Item2 + x] = -1;
            }

            if (free.Item2 == taken.Item3)
                freeSpace.Remove(free);
            else
                freeSpace[freeSpace.IndexOf(free)] = (free.Item1 + taken.Item3, free.Item2 - taken.Item3);
        }

        for (var x = 0; x < fileList.Count; x++)
        {
            if (fileList[x] == -1) continue;

            sum += (x * fileList[x]);
        }
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
