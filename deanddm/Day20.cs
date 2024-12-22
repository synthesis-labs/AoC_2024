using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day20
    {
        String[] lines = File.ReadAllLines("Inputs/input.day20.txt");
        char[,] map = new char[0, 0];
        (int row, int column) start = new();
        (int row, int column) end = new();
        List<(int row, int column)> directions = new List<(int row, int column)>() { (-1, 0), (1, 0), (0, -1), (0, 1) };
        List<(int row, int column)> cheats = new List<(int row, int column)>();
        Dictionary<int, int> results = new Dictionary<int, int>();
        int intialSteps = 0;

        internal void ExecuteDay20Part1()
        {
            ParseInput();
            intialSteps = BFS();
            GeneratePotentialCheats();
            RunBFSForWithCheats();
            var savings = results.Where(x => x.Key >= 100).Select(x=> x.Value).Sum();
            Console.WriteLine("Day 20, Part 1: " + savings);
        }

        internal void ExecuteDay20Part2()
        {
            Console.WriteLine("Day 20, Part 2: ");
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];

            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];

                    if (map[i, j] == 'S')
                        start = (i, j);

                    if (map[i, j] == 'E')
                        end = (i, j);
                }
            }
        }

        private void GeneratePotentialCheats()
        {
            for (int i = 1; i < map.GetLength(0) - 1; i++)
            {
                for (int j = 1; j < map.GetLength(1) - 1; j++)
                {
                    if (map[i, j] == '#')
                        cheats.Add((i, j));
                }
            }
        }

        private void RunBFSForWithCheats()
        {
            foreach (var cheat in cheats)
            {
                map[cheat.row, cheat.column] = '.';

                int steps = intialSteps - BFS();

                if (results.ContainsKey(steps))
                    results[steps]++;
                else
                    results[steps] = 1;

                map[cheat.row, cheat.column] = '#';
            }
        }

        private int BFS()
        {
            var queue = new Queue<((int row, int col) position, int steps)>();
            var visited = new HashSet<(int row, int col)>() { start };

            queue.Enqueue((start, 0));

            while (queue.Count > 0)
            {
                var (currentPosition, steps) = queue.Dequeue();

                if (currentPosition == end)
                    return steps;

                foreach (var (row, column) in directions)
                {
                    var neighbor = (row: currentPosition.row + row, column: currentPosition.col + column);

                    if (InsideBounds(neighbor.row, neighbor.column) &&
                        map[neighbor.row, neighbor.column] != '#' &&
                        !visited.Contains(neighbor))
                    {
                        visited.Add(neighbor);
                        queue.Enqueue((neighbor, steps + 1));
                    }
                }
            }

            return -1; // No path found
        }

        private bool InsideBounds(int nextRow, int nextColumn) =>
            nextRow >= 0 || nextRow < map.GetLength(0) || nextColumn >= 0 || nextColumn < map.GetLength(1);
    }
}
