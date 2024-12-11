using System.Linq;

internal class Day09: IDay
{
    public double Part1(List<string> data)
    {
        var nums = data[0].Select(c => int.Parse($"{c}"));
        var files = nums.Select((n, i) => (n, i)).Where(x => x.i % 2 == 0).Select((x, i) => (listIndex: x.i, fileIndex: i, size: x.n));
        var expandedFiles = files.SelectMany(f => Enumerable.Range(0, f.size).Select(x => (f.listIndex, f.fileIndex)));
        var spaces = nums.Select((n, i) => (n, i)).Where(x => x.i % 2 != 0).Select((x, i) => (listIndex: x.i, fileIndex: -1, size: x.n));
        var expandedSpaces = spaces.SelectMany(f => Enumerable.Range(0, f.size).Select(x => (f.listIndex, f.fileIndex)));
        var blocks = expandedFiles.Concat(expandedSpaces).OrderBy(x => x.listIndex).ToList();
                       
        var fIdx = blocks.Select((b, i) => (b, i)).Where(x => x.b.fileIndex > -1).Select(x => x.i).ToList();
        var fbi = fIdx.Count() - 1;
        var eIdx = blocks.Select((b,i) => (b,i)).Where(x => x.b.fileIndex == -1).Select(x => x.i).ToList();
        foreach (int s in eIdx)
        {
            var ff = fIdx[fbi];
            if (ff > s)
            {
                var swap = blocks[ff];
                blocks[ff] = blocks[s];
                blocks[s] = swap;
            }
            fbi--;
        }
        return blocks.Select((x, i) => (x.fileIndex, listIndex: i)).Where(x => x.fileIndex > -1).Aggregate(0d, (sum, file) => sum += file.listIndex * (double)file.fileIndex);
    }

    public double Part2(List<string> data)
    {
        var nums = data[0].Select(c => int.Parse($"{c}"));
        var files = nums.Select((n, i) => (n, i)).Where(x => x.i % 2 == 0).Select((x, i) => (listIndex: x.i, fileIndex: i, size: x.n));
        var expandedFiles = files.SelectMany(f => Enumerable.Range(0, f.size).Select(x => (f.listIndex, f.fileIndex)));
        var spaces = nums.Select((n, i) => (n, i)).Where(x => x.i % 2 != 0).Select((x, i) => (listIndex: x.i, fileIndex: (i+2)*(-1), size: x.n));
        var expandedSpaces = spaces.SelectMany(f => Enumerable.Range(0, f.size).Select(x => (f.listIndex, f.fileIndex)));
        var blocks = expandedFiles.Concat(expandedSpaces).OrderBy(x => x.listIndex).Select((x, i) => (listIndex: i, x.fileIndex)).ToList();
        var fileGroup = blocks.GroupBy(x => x.fileIndex).Where(g => g.Key >= 0).OrderByDescending(g => g.Key);

        foreach (var fg in fileGroup)
        {
            var len = fg.Count();
            if (len > 0)
            {
                var spaceGroup = blocks.GroupBy(x => x.fileIndex).Where(g => g.Key < -1).OrderByDescending(g => g.Key);
                var s = spaceGroup.Where(sg => sg.Count() >= fg.Count() && sg.Last().listIndex < fg.First().listIndex).FirstOrDefault();
                if (s != null)
                {
                   for (var i = 0; i < fg.Count(); i++)
                   {
                        var fi = fg.ElementAt(i).listIndex;
                        var si = s.ElementAt(i).listIndex;
                        var swap = blocks[si];
                        blocks[si] = blocks[fi];
                        blocks[fi] = (swap.listIndex, -1);
                   }
                }
            }
        }  

        return blocks.Select((x, i) => (x.fileIndex, listIndex: i)).Where(x => x.fileIndex > -1).Aggregate(0d, (sum, file) => sum += file.listIndex * (double)file.fileIndex);
    }
}