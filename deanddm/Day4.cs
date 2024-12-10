using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day4
    {
        String[] lines = File.ReadAllLines("Inputs/input.day4.txt");
        (int, int)[] Coordinates =
        {
                (-1, 0), (1, 0), (0, -1), (0, 1), // Vertical & Horizontal
                (-1, -1), (-1, 1), (1, -1), (1, 1) // Diagonal
            };

        internal void ExecuteDay4Part1()
        {
            int occurrences = CountOccurrencesOfXMAS();
            Console.WriteLine("Day 4, Part 1: " + occurrences);
        }

        internal void ExecuteDay4Part2()
        {
            int occurrences = CountOccurrencesOfX();
            Console.WriteLine("Day 4, Part 2: " + occurrences);
        }

        private int CountOccurrencesOfXMAS()
        {
            return lines.SelectMany((line, row) =>
                    line.SelectMany((_, col) =>
                        Coordinates.Where(coordinate => MatchesWord(row, col, coordinate))))
                .Count();
        }

        private bool MatchesWord(int startRow, int startColumn, (int row, int column) coordinate)
        {
            string word = "XMAS";
            for (int i = 0; i < word.Length; i++)
            {
                int nextRow = startRow + i * coordinate.row;
                int nextColumn = startColumn + i * coordinate.column;

                if (nextRow < 0 || nextRow >= lines.Length || nextColumn < 0 || nextColumn >= lines[nextRow].Length || lines[nextRow][nextColumn] != word[i])
                    return false;
            }
            return true;
        }

        private int CountOccurrencesOfX()
        {
            return lines.SelectMany((line, row) =>
                    line.SelectMany((_, column) =>
                        lines[row][column] == 'A' &&
                        MatchesXShape(row, column) ? new[] { 1 } : Enumerable.Empty<int>()))
                .Count();
        }

        private bool MatchesXShape(int centerRow, int centerColumn)
        {
            string word = "MAS";

            bool left =
                   (IsValidCell(centerRow - 1, centerColumn - 1) && lines[centerRow - 1][centerColumn - 1] == word[0] &&
                   IsValidCell(centerRow + 1, centerColumn + 1) && lines[centerRow + 1][centerColumn + 1] == word[2]) ||
                   (IsValidCell(centerRow - 1, centerColumn - 1) && lines[centerRow - 1][centerColumn - 1] == word[2] &&
                   IsValidCell(centerRow + 1, centerColumn + 1) && lines[centerRow + 1][centerColumn + 1] == word[0]);

            bool right =
                   (IsValidCell(centerRow - 1, centerColumn + 1) && lines[centerRow - 1][centerColumn + 1] == word[0] &&
                   IsValidCell(centerRow + 1, centerColumn - 1) && lines[centerRow + 1][centerColumn - 1] == word[2]) ||
                   (IsValidCell(centerRow - 1, centerColumn + 1) && lines[centerRow - 1][centerColumn + 1] == word[2] &&
                   IsValidCell(centerRow + 1, centerColumn - 1) && lines[centerRow + 1][centerColumn - 1] == word[0]);

            return left && right;
        }

        private bool IsValidCell(int row, int col)
        {
            return row >= 0 && row < lines.Length && col >= 0 && col < lines[row].Length;
        }
    }
}
