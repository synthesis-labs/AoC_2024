using Utilities;
namespace AdventOfCode;

public class Day04 : BaseDay
{
    private readonly string[] _input;

    public Day04()
    {
        _input = File.ReadAllLines(InputFilePath);
    }
    private string ProcessInput1(string[] input)
    {
        int sum = 0;
        for(var i = 0; i < input.Length; i++)
        {
            for(var j = 0; j < input[i].Length; j++)
            {
                if (input[i][j] == 'X')
                {
                    if (FindValid(i, j, input, 'M', CompassDirection.N))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.NE))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.E))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.SE))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.S))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.SW))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.W))
                        sum++;

                    if (FindValid(i, j, input, 'M', CompassDirection.NW))
                        sum++;
                }
 
            }
        }
        return $"{sum}";
    }
    private bool FindValid(int row, int column, string[] input, char nextChar, CompassDirection direction)
    {
        switch (direction)
        {
            case CompassDirection.N:
                if (row - 1 > -1 && input[row - 1][column] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if(nextChar == 'M')
                        return FindValid(row - 1, column, input, 'A', direction);
                    else if(nextChar == 'A')
                        return FindValid(row - 1, column, input, 'S', direction);
                break;
            case CompassDirection.NE:
                if (row - 1 > -1 && column + 1 < input[row].Length && input[row - 1][column + 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row - 1, column + 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row - 1, column + 1, input, 'S', direction);
                break;
            case CompassDirection.E:
                if (column + 1 < input[row].Length && input[row][column + 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row, column + 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row, column + 1, input, 'S', direction);
                break;
            case CompassDirection.SE:
                if (row + 1 < input.Length && column + 1 < input[row].Length && input[row + 1][column + 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row + 1, column + 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row + 1, column + 1, input, 'S', direction);
                break;
            case CompassDirection.S:
                if (row + 1 < input.Length && input[row + 1][column] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row + 1, column, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row + 1, column, input, 'S', direction);
                break;
            case CompassDirection.SW:
                if (row + 1 < input.Length && column - 1 > -1 && input[row + 1][column - 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row + 1, column - 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row + 1, column - 1, input, 'S', direction);
                break;
            case CompassDirection.W:
                if (column - 1 > -1 && input[row][column - 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row, column - 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row, column - 1, input, 'S', direction);
                break;
            case CompassDirection.NW:
                if (row - 1 > -1 && column - 1 > -1 && input[row - 1][column - 1] == nextChar)
                    if (nextChar == 'S')
                        return true;
                    else if (nextChar == 'M')
                        return FindValid(row - 1, column - 1, input, 'A', direction);
                    else if (nextChar == 'A')
                        return FindValid(row - 1, column - 1, input, 'S', direction);
                break;

        }
        return false;
    }


    private string ProcessInput2(string[] input)
    {
        int sum = 0;
        for (var i = 0; i < input.Length; i++)
        {
            for (var j = 0; j < input[i].Length; j++)
            {
                if (input[i][j] == 'A')
                {
                    var right = false;
                    var left = false;
                    if ((i - 1 >= 0 && j + 1 < input[i].Length && input[i - 1][j + 1] == 'M' &&
                        i + 1 < input.Length && j - 1 > -1 && input[i + 1][j - 1] == 'S') || 
                        (i - 1 >= 0 && j + 1 < input[i].Length && input[i - 1][j + 1] == 'S' &&
                        i + 1 < input.Length && j - 1 > -1 && input[i + 1][j - 1] == 'M'))
                        right = true;

                    if ((i - 1 > -1 && j - 1 > - 1 && input[i - 1][j - 1] == 'M' &&
                        i + 1 < input.Length && j + 1 < input[i].Length && input[i + 1][j + 1] == 'S') ||
                        (i - 1 > -1 && j - 1 > -1 && input[i - 1][j - 1] == 'S' &&
                        i + 1 < input.Length && j + 1 < input[i].Length && input[i + 1][j + 1] == 'M'))
                        left = true;

                    if (right && left)
                        sum++;
                }

            }
        }
        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
