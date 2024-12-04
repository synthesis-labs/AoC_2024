internal class Day04 : IDay
{
    private int _count, _xcount = 0;
    public Day04(List<string> data)
    {
        var (rows, cols) = (data.Count, data.First().Length);
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
            {
                var (count, xcount) = CrossWord(data, r, c, rows, cols);
                _count += count;
                _xcount += xcount == 2 ? 1 : 0;
            }
    }

    public async Task<int> Part1(List<string> data) => _count;

    public async Task<int> Part2(List<string> data) => _xcount;

    private (int count, int xcount) CrossWord(List<string> puzzle, int row, int col, int maxr, int maxc)
    {
        var (count, xcount) = (0, 0);
        if (puzzle[row][col] == 'X')
        {
            if (col + 3 < maxc && puzzle[row][col + 1] == 'M' && puzzle[row][col + 2] == 'A' && puzzle[row][col + 3] == 'S') count++;
            if (col - 3 >= 0 && puzzle[row][col - 1] == 'M' && puzzle[row][col - 2] == 'A' && puzzle[row][col - 3] == 'S') count++;
            if (row + 3 < maxr && puzzle[row + 1][col] == 'M' && puzzle[row + 2][col] == 'A' && puzzle[row + 3][col] == 'S') count++;
            if (row - 3 >= 0 && puzzle[row - 1][col] == 'M' && puzzle[row - 2][col] == 'A' && puzzle[row - 3][col] == 'S') count++;
            if (row + 3 < maxr && col + 3 < maxc && puzzle[row + 1][col + 1] == 'M' && puzzle[row + 2][col + 2] == 'A' && puzzle[row + 3][col + 3] == 'S') count++;
            if (row - 3 >= 0 && col - 3 >= 0 && puzzle[row - 1][col - 1] == 'M' && puzzle[row - 2][col - 2] == 'A' && puzzle[row - 3][col - 3] == 'S') count++;
            if (row - 3 >= 0 && col + 3 < maxc && puzzle[row - 1][col + 1] == 'M' && puzzle[row - 2][col + 2] == 'A' && puzzle[row - 3][col + 3] == 'S') count++;
            if (row + 3 < maxr && col - 3 >= 0 && puzzle[row + 1][col - 1] == 'M' && puzzle[row + 2][col - 2] == 'A' && puzzle[row + 3][col - 3] == 'S') count++;
        }
        if (puzzle[row][col] == 'A' && row - 1 >= 0 && col - 1 >= 0 && row + 1 < maxr && col + 1 < maxc)
        {
            if (puzzle[row + 1][col + 1] == 'S' && puzzle[row - 1][col - 1] == 'M') xcount++;
            if (puzzle[row + 1][col + 1] == 'M' && puzzle[row - 1][col - 1] == 'S') xcount++;
            if (puzzle[row - 1][col + 1] == 'S' && puzzle[row + 1][col - 1] == 'M') xcount++;
            if (puzzle[row - 1][col + 1] == 'M' && puzzle[row + 1][col - 1] == 'S') xcount++;
        }
        return (count, xcount);
    }
}