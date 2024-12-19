using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day18
    {
        String[] lines = File.ReadAllLines("Inputs/input.day18.txt");
        char[,] map = new char[71, 71];
        List<(int row, int column)> bytes = new List<(int row, int column)>();
        (int row, int column) start = (0, 0);
        (int row, int column) end = (70, 70);
        List<(int row, int column)> directions = new List<(int row, int col)>()
        {
            (-1, 0), (1, 0), (0, -1), (0, 1)
        };

        internal void ExecuteDay18Part1()
        {
            ParseInput();
            UpdateMap(1024);
            //PrintTheMap(map);
            int steps = FindShortestPath();
            Console.WriteLine("Day 18, Part 1: " + steps);
        }

        internal void ExecuteDay18Part2()
        {
            (int row, int column) blocker = new();
            for (int i = 1024; i < bytes.Count; i++)
            {
                UpdateMap(i + 1);
                int steps = FindShortestPath();

                if (steps == -1)
                {
                    blocker = bytes[i];
                    break;
                }
            }
            Console.WriteLine("Day 18, Part 2: " + $"{ blocker.column },{ blocker.row }");
        }

        private void ParseInput()
        {
            foreach (var line in lines)
            {
                var split = line.Split(',', StringSplitOptions.RemoveEmptyEntries);
                var x = Int32.Parse(split[0]);
                var y = Int32.Parse(split[1]);
                bytes.Add((y, x));
            }
        }

        private void UpdateMap(int bytesToFall)
        {
            end = (map.GetLength(0) - 1, map.GetLength(1) - 1);
            for (int i = 0; i < map.GetLength(0); i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                {
                    if (bytes.Take(bytesToFall).Contains((i, j)))
                        map[i, j] = '#';
                    else
                        map[i, j] = '.';
                }
            }
        }

        private void PrintTheMap(char[,] mapToPrint)
        {
            for (int i = 0; i < mapToPrint.GetLength(0); i++)
            {
                for (int j = 0; j < mapToPrint.GetLength(1); j++)
                    Console.Write(mapToPrint[i, j]);

                Console.WriteLine();
            }
        }

        private int FindShortestPath()
        {
            var queue = new Queue<((int row, int column) position, int steps)>();
            var seen = new HashSet<(int row, int column)>();

            queue.Enqueue((start, 0));
            seen.Add(start);

            while (queue.Count > 0)
            {
                var (current, steps) = queue.Dequeue();
                if (current == end)
                    return steps;

                foreach (var direction in directions)
                {
                    var next = (current.row + direction.row, current.column + direction.column);
                    if (IsValidMove(next, seen))
                    {
                        queue.Enqueue((next, steps + 1));
                        seen.Add(next);
                    }
                }
            }

            return -1;
        }

        private bool IsValidMove((int row, int column) move, HashSet<(int row, int column)> seen)
        {
            return move.row >= 0 && move.row < map.GetLength(0) &&
                   move.column >= 0 && move.column < map.GetLength(1) &&
                   map[move.row, move.column] == '.' &&
                   !seen.Contains(move); 
        }
    }
}
