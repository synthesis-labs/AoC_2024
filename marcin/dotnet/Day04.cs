internal class Day04 : IDay
{
    public async Task<int> Part1(List<string> data)
    {
        var (rows, cols) = (data.Count, data.First().Length);
        int result = 0;

        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                result += CheckWord(data, r, c, rows, cols);

        return result;
    }

    public async Task<int> Part2(List<string> data)
    {
        var (rows, cols) = (data.Count, data.First().Length);
        int result = 0;

        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
            {
                var count = CrossWord(data, r, c, rows, cols);
                if (count == 2) result++;
            }

        return result;
    }

    private int CheckWord(List<string> puzzle, int row, int col, int maxr, int maxc)
    {
        var count = 0;
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
        return count;
    }

    private int CrossWord(List<string> puzzle, int row, int col, int maxr, int maxc)
    {
        var count = 0;
        if (puzzle[row][col] == 'A' && row - 1 >= 0 && col - 1 >= 0 && row + 1 < maxr && col + 1 < maxc)
        {
            if (puzzle[row + 1][col + 1] == 'S' && puzzle[row - 1][col - 1] == 'M') count++;
            if (puzzle[row + 1][col + 1] == 'M' && puzzle[row - 1][col - 1] == 'S') count++;
            if (puzzle[row - 1][col + 1] == 'S' && puzzle[row + 1][col - 1] == 'M') count++;
            if (puzzle[row - 1][col + 1] == 'M' && puzzle[row + 1][col - 1] == 'S') count++;
        }
        return count;
    }
}