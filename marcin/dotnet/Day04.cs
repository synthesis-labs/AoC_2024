internal class Day04 : IDay
{
    public async Task<int> Part1(List<string> data) => CrossWord(data).words;

    public async Task<int> Part2(List<string> data) => CrossWord(data).crosses;

    private (int words, int crosses) CrossWord(List<string> puzzle)
    {
        var (words, crosses, maxr, maxc) = (0, 0, puzzle.Count, puzzle.First().Length);
        for (int row = 0; row < maxr; row++)
            for (int col = 0; col < maxc; col++)
            {
                var xcount = 0;
                if (puzzle[row][col] == 'X')
                {
                    if (col + 3 < maxc && puzzle[row][col + 1] == 'M' && puzzle[row][col + 2] == 'A' && puzzle[row][col + 3] == 'S') words++;
                    if (col - 3 >= 0 && puzzle[row][col - 1] == 'M' && puzzle[row][col - 2] == 'A' && puzzle[row][col - 3] == 'S') words++;
                    if (row + 3 < maxr && puzzle[row + 1][col] == 'M' && puzzle[row + 2][col] == 'A' && puzzle[row + 3][col] == 'S') words++;
                    if (row - 3 >= 0 && puzzle[row - 1][col] == 'M' && puzzle[row - 2][col] == 'A' && puzzle[row - 3][col] == 'S') words++;
                    if (row + 3 < maxr && col + 3 < maxc && puzzle[row + 1][col + 1] == 'M' && puzzle[row + 2][col + 2] == 'A' && puzzle[row + 3][col + 3] == 'S') words++;
                    if (row - 3 >= 0 && col - 3 >= 0 && puzzle[row - 1][col - 1] == 'M' && puzzle[row - 2][col - 2] == 'A' && puzzle[row - 3][col - 3] == 'S') words++;
                    if (row - 3 >= 0 && col + 3 < maxc && puzzle[row - 1][col + 1] == 'M' && puzzle[row - 2][col + 2] == 'A' && puzzle[row - 3][col + 3] == 'S') words++;
                    if (row + 3 < maxr && col - 3 >= 0 && puzzle[row + 1][col - 1] == 'M' && puzzle[row + 2][col - 2] == 'A' && puzzle[row + 3][col - 3] == 'S') words++;
                }
                if (puzzle[row][col] == 'A' && row - 1 >= 0 && col - 1 >= 0 && row + 1 < maxr && col + 1 < maxc)
                {
                    if (puzzle[row + 1][col + 1] == 'S' && puzzle[row - 1][col - 1] == 'M') xcount++;
                    if (puzzle[row + 1][col + 1] == 'M' && puzzle[row - 1][col - 1] == 'S') xcount++;
                    if (puzzle[row - 1][col + 1] == 'S' && puzzle[row + 1][col - 1] == 'M') xcount++;
                    if (puzzle[row - 1][col + 1] == 'M' && puzzle[row + 1][col - 1] == 'S') xcount++;
                }
                crosses += xcount == 2 ? 1 : 0;
            }
        return (words, crosses);
    }
}