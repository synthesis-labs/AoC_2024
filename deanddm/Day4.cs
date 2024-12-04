using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day4
    {
        String[] lines = File.ReadAllLines("input.day4.txt");
        int occurrences = 0;
        int masOccurrences = 0;

        internal void ExecuteDay4Part1()
        {
            FindOccurrences();

            Console.WriteLine("Day 4, Part 1: " + occurrences);
        }

        internal void ExecuteDay4Part2()
        {
            FindMasOccurrences();

            Console.WriteLine("Day 4, Part 2: " + masOccurrences);
        }

        private void FindOccurrences()
        {
            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    if (lines[i][j] == 'X')
                    {
                        if (LookUp(i, j, 'M'))
                            if (LookUp(i - 1, j, 'A'))
                                if (LookUp(i - 2, j, 'S'))
                                    occurrences++;

                        if (LookUpRight(i, j, 'M'))
                            if (LookUpRight(i - 1, j + 1, 'A'))
                                if (LookUpRight(i - 2, j + 2, 'S'))
                                    occurrences++;

                        if (LookRight(i, j, 'M'))
                            if (LookRight(i, j + 1, 'A'))
                                if (LookRight(i, j + 2, 'S'))
                                    occurrences++;

                        if (LookDownRight(i, j, 'M'))
                            if (LookDownRight(i + 1, j + 1, 'A'))
                                if (LookDownRight(i + 2, j + 2, 'S'))
                                    occurrences++;

                        if (LookDown(i, j, 'M'))
                            if (LookDown(i + 1, j, 'A'))
                                if (LookDown(i + 2, j, 'S'))
                                    occurrences++;

                        if (LookDownLeft(i, j, 'M'))
                            if (LookDownLeft(i + 1, j - 1, 'A'))
                                if (LookDownLeft(i + 2, j - 2, 'S'))
                                    occurrences++;

                        if (LookLeft(i, j, 'M'))
                            if (LookLeft(i, j - 1, 'A'))
                                if (LookLeft(i, j - 2, 'S'))
                                    occurrences++;

                        if (LookUpLeft(i, j, 'M'))
                            if (LookUpLeft(i - 1, j - 1, 'A'))
                                if (LookUpLeft(i - 2, j - 2, 'S'))
                                    occurrences++;
                    }
                }
            }
        }

        private void FindMasOccurrences()
        {
            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    if (lines[i][j] == 'A')
                    {
                        bool foundRight = false;

                        if (LookUpRight(i, j, 'M'))
                            if (LookDownLeft(i, j, 'S'))
                            {
                                foundRight = true;
                            }

                        if (LookUpRight(i, j, 'S'))
                            if (LookDownLeft(i, j, 'M'))
                            {
                                foundRight = true;
                            }

                        bool foundLeft = false;

                        if (LookUpLeft(i, j, 'M'))
                            if (LookDownRight(i, j, 'S'))
                            {
                                foundLeft = true;
                            }

                        if (LookUpLeft(i, j, 'S'))
                            if (LookDownRight(i, j, 'M'))
                            {
                                foundLeft = true;
                            }

                        if (foundLeft && foundRight)
                            masOccurrences++;
                    }
                }
            }
        }

        private bool LookUp(int rowNumber, int columnNumber, char character)
        {
            //Look up
            if (rowNumber - 1 >= 0 && lines[rowNumber - 1][columnNumber] == character)
                return true;

            return false;
        }

        private bool LookUpRight(int rowNumber, int columnNumber, char character)
        {
            //Look right
            if ((rowNumber - 1 >= 0) &&
                (columnNumber + 1) <= (lines[rowNumber].Length - 1) &&
                lines[rowNumber - 1][columnNumber + 1] == character)
                return true;

            return false;
        }

        private bool LookRight(int rowNumber, int columnNumber, char character)
        {
            //Look right
            if ((columnNumber + 1) <= (lines[rowNumber].Length - 1) && lines[rowNumber][columnNumber + 1] == character)
                return true;

            return false;
        }

        private bool LookDownRight(int rowNumber, int columnNumber, char character)
        {
            //Look right
            if (((rowNumber + 1) <= (lines.Length - 1)) &&
                (columnNumber + 1) <= (lines[rowNumber].Length - 1) &&
                lines[rowNumber + 1][columnNumber + 1] == character)
                return true;

            return false;
        }

        private bool LookDown(int rowNumber, int columnNumber, char character)
        {
            //Look down
            if ((rowNumber + 1) <= (lines.Length - 1) && lines[rowNumber + 1][columnNumber] == character)
                return true;

            return false;
        }

        private bool LookDownLeft(int rowNumber, int columnNumber, char character)
        {
            //Look left
            if (((rowNumber + 1) <= (lines.Length - 1)) &&
                (columnNumber - 1) >= 0 &&
                lines[rowNumber + 1][columnNumber - 1] == character)
                return true;

            return false;
        }

        private bool LookLeft(int rowNumber, int columnNumber, char character)
        {
            //Look left
            if ((columnNumber - 1) >= 0 && lines[rowNumber][columnNumber - 1] == character)
                return true;

            return false;
        }

        private bool LookUpLeft(int rowNumber, int columnNumber, char character)
        {
            //Look left
            if ((rowNumber - 1 >= 0) &&
                (columnNumber - 1) >= 0 &&
                lines[rowNumber - 1][columnNumber - 1] == character)
                return true;

            return false;
        }
    }
}
