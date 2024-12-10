using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day8
    {
        String[] lines = File.ReadAllLines("Inputs/input.day8.txt");
        char[,] map = new char[0, 0];
        Dictionary<char, List<(int, int)>> antennas = new Dictionary<char, List<(int, int)>>();
        HashSet<(int, int, char)> antiNodes = new HashSet<(int, int, char)>();
        Dictionary<char, int> nodeCounts = new Dictionary<char, int>();
        int antiNodeCount = 0;

        internal void ExecuteDay8Part1()
        {
            ParseInput();
            GenerateAntiNodes();
            var counts = antiNodes.Select(x => (x.Item1, x.Item2)).Distinct().Count();
            Console.WriteLine("Day 8, Part 1: " + counts);
        }

        internal void ExecuteDay1Part2()
        {
            antiNodeCount = 0;
            GenerateAntiNodes(true);
            antennas.Where(x => x.Value.Count > 2).Select(x => (x.Key, x.Value.Count)).ToList().ForEach(x => nodeCounts.Add(x.Key, x.Count));

            int hashCount = 0;
            for (int i = 0; i < map.GetLength(0);  i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                {
                    if (map[i, j] == '#')
                        hashCount++;
                }
            }

            var counts = nodeCounts.Sum(x => x.Value) + hashCount;
            Console.WriteLine("Day 8, Part 2: " + counts);
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];
            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];

                    if (Char.IsLetterOrDigit(lines[i][j]))
                    {
                        if (antennas.ContainsKey(map[i, j]))
                            antennas[map[i, j]].Add((i, j));
                        else
                            antennas.Add(lines[i][j], new List<(int, int)>() { (i, j) });
                    }
                }
            }
        }

        private void GenerateAntiNodes(bool includeHarmonics = false)
        {
            foreach (var antenna in antennas)
            {
                foreach (var location in antenna.Value)
                {
                    var newList = new List<(int, int)>();
                    Enumerable.Range(0, antenna.Value.Count - 1).ToList().ForEach(x => newList.Add(location));
                    var excludeList = antenna.Value.Where(x => x != location);
                    var zipList = newList.Zip(excludeList).ToList();
                    newList.Zip(excludeList).ToList().ForEach(x =>
                    {
                        var rowDistance = Math.Abs(x.First.Item1 - x.Second.Item1);
                        var columnDistance = Math.Abs(x.First.Item2 - x.Second.Item2);
                        bool above = x.First.Item1 < x.Second.Item1;
                        bool left = x.First.Item2 < x.Second.Item2;

                        (int topRow, int topColumn) top = (x.First.Item1, x.First.Item2);
                        (int bottomRow, int bottomColumn) bottom = (x.Second.Item1, x.Second.Item2);

                        var (createdNodes, newTop, newBottom) = CreateAntiNodes(above, left, rowDistance, columnDistance, top, bottom, antenna.Key);
                        top = newTop;
                        bottom = newBottom;

                        if (includeHarmonics)
                        {
                            while (true)
                            {
                                (createdNodes, newTop, newBottom) = CreateAntiNodes(above, left, rowDistance, columnDistance, top, bottom, antenna.Key);

                                if (!createdNodes)
                                    break;

                                top = newTop;
                                bottom = newBottom;
                            }
                        }
                    });
                }
            }
        }

        private (bool, (int topRow, int topColumn) top, (int bottomRow, int bottomColumn) bottom) CreateAntiNodes(bool above, bool left, int rowDistance, int columnDistance, (int topRow, int topColumn) top, (int bottomRow, int bottomColumn) bottom, char antenna)
        {
            bool createdNodes = false;

            if (above)
            {
                top.topRow -= rowDistance;
                bottom.bottomRow += rowDistance;
            }
            else
            {
                top.topRow += rowDistance;
                bottom.bottomRow -= rowDistance;
            }

            if (left)
            {
                top.topColumn -= columnDistance;
                bottom.bottomColumn += columnDistance;
            }
            else
            {
                top.topColumn += columnDistance;
                bottom.bottomColumn -= columnDistance;
            }

            if (!OutOfBounds(top) && !LocationExists(top, antenna))
            {
                AddToMap(top);

                if (antiNodes.Add((top.topRow, top.topColumn, antenna)))
                    antiNodeCount++;

                createdNodes = true;
            }

            if (!OutOfBounds(bottom) && !LocationExists(bottom, antenna))
            {
                AddToMap(bottom);

                if (antiNodes.Add((bottom.bottomRow, bottom.bottomColumn, antenna)))
                    antiNodeCount++;

                createdNodes = true;
            }

            return (createdNodes, top, bottom);
        }

        private bool OutOfBounds((int row, int column) antiNode) =>
            antiNode.row < 0 || antiNode.row >= lines.Length || antiNode.column < 0 || antiNode.column >= lines[0].Length;

        private bool LocationExists((int row, int column) antiNode, char antenna) =>
            map[antiNode.row, antiNode.column] == antenna;

        private void AddToMap((int row, int column) antiNode) =>
            map[antiNode.row, antiNode.column] = Char.IsLetterOrDigit(map[antiNode.row, antiNode.column]) ? map[antiNode.row, antiNode.column] : '#';
    }
}
