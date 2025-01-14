using Utilities;

namespace AdventOfCode;

public class Day09 : BaseDay
{
    int[] fullIterations = [];
    List<(int id, int index, int length)> reserved = [];
    List<(int index, int length)> freeSpace = [];

    private readonly string _input;
    public Day09()
    {
        _input = File.ReadAllText(InputFilePath);

        fullIterations = new int[_input.ToIntList().Sum()];

        var ind = 0;
        var curblock = -1;
        for (var x = 0; x < _input.Length; x++)
        {
            int c = _input[x] - '0';
            bool space = x % 2 == 1;

            if(!space)
            {
                curblock++;
                reserved.Add((curblock, ind, c));
            }
            else if (c != 0)
            {
                freeSpace.Add((ind, c));
            }

            for (int y = 0; y < c; y++)
            {
                fullIterations[ind++] = space ? -1 : curblock;
            }
        }
    }
    private string ProcessInput1()
    {
        var fileList = new List<int>(fullIterations);
        int start = 0;
        int end = fullIterations.Length - 1;
        long sum = 0;

        while (start < end)
        {
            if (fileList[start] != -1)
            {
                start++;
                continue;
            }

            while (fileList[end] == -1) end--;
            fileList[start++] = fileList[end];
            fileList[end--] = -1;
        }

        for (var x = 0; x < fileList.Count; x++)
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
        for(int z = reserved.Count -1; z >= 0; z--)
        {
            var taken = reserved[z];
            for(int y = 0; y < freeSpace.Count; y++)
            {
                var free = freeSpace[y];
                if (free.index >= taken.index) break;
                if (free.length < taken.length) continue;
            
                for (var x = 0; x < taken.length; x++)
                {
                    fileList[free.index + x] = taken.id;
                    fileList[taken.index + x] = -1;
                }

                if (free.length == taken.length)
                    freeSpace.Remove(free);
                else
                    freeSpace[freeSpace.IndexOf(free)] = (free.index + taken.length, free.length - taken.length);
                break;
            }
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