
using System.Collections.Generic;

namespace AOC2024
{
    internal class Day12
    {
        String[] lines = File.ReadAllLines("Inputs/input.day12.txt");
        char[,] map = new char[0, 0];
        Dictionary<int, List<(char character, int row, int column)>> regions = [];
        List<List<(int row, int column, bool matchCharacter)>> combinedAngles =
        [
            new List<(int row, int column, bool matchCharacter)> { (0, -1, false), (-1, 0, false) }, //outUpLeft
            new List<(int row, int column, bool matchCharacter)> { (-1, 0, false), (0, 1, false) }, //outUpRight
            new List<(int row, int column, bool matchCharacter)> { (0, -1, false), (1, 0, false) }, //outDownLeft
            new List<(int row, int column, bool matchCharacter)> { (0, 1, false), (1, 0, false) }, //outDownRight
            new List<(int row, int column, bool matchCharacter)> { (0, -1, true), (-1, -1, false), (-1, 0, true) }, //inDownRight
            new List<(int row, int column, bool matchCharacter)> { (0, 1, true), (-1, 1, false), (-1, 0, true) }, //inDownLeft
            new List<(int row, int column, bool matchCharacter)> { (0, -1, true), (1, -1, false), (1, 0, true) }, //inUpRight
            new List<(int row, int column, bool matchCharacter)> { (0, 1, true), (1, 1, false), (1, 0, true) } //inUpLeft
        ];
        List<(int row, int column)> directions = [(-1, 0), (0, 1), (1, 0), (0, -1)];

        internal void ExecuteDay12Part1()
        {
            ParseInput();
            IdentifyRegions();
            int totalPrice = CalculateBoundaries();
            Console.WriteLine("Day 12, Part 1: " + totalPrice);
        }

        internal void ExecuteDay12Part2()
        {
            ParseInput();
            int totalPrice = CalculateSides();
            Console.WriteLine("Day 12, Part 2: " + totalPrice);
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];

            for (int i = 0; i < lines.Length; i++)
                for (int j = 0; j < lines[i].Length; j++)
                    map[i, j] = lines[i][j];
        }

        private void IdentifyRegions()
        {
            int region = 0;
            for (int i = 0; i < map.GetLength(0); i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                {
                    if (map[i, j] == '.')
                        continue;

                    var plots = new List<(char character, int row, int column)>() { (character: map[i, j], row: i, column: j) };
                    FindConnectedGardens(plots);
                    regions.Add(region, plots.Distinct().ToList());
                    region++;
                }
            }
        }

        private void FindConnectedGardens(List<(char character, int row, int column)> plots)
        {
            var newPlots = new List<(char character, int row, int column)>();
            foreach (var garden in plots)
            {
                if (map[garden.row, garden.column] == '.')
                    continue;

                foreach (var direction in directions)
                {
                    if (!OutsideBounds(garden.row + direction.row, garden.column + direction.column) && 
                        map[garden.row + direction.row, garden.column + direction.column] == garden.character)
                        newPlots.Add((garden.character, garden.row + direction.row, garden.column + direction.column));
                }

                map[garden.row, garden.column] = '.';
            }

            if (newPlots.Count > 0)
            {
                plots.AddRange(newPlots);
                FindConnectedGardens(plots);
            }
        }

        private int CalculateBoundaries()
        {
            int totalPrice = 0;
            foreach (var region in regions)
            {
                int boundaries = 0;
                if (region.Value.Count == 1)
                {
                    boundaries = 4;
                }
                else
                {
                    foreach (var plot in region.Value)
                    {
                        foreach (var direction in directions)
                        {
                            if (region.Value.Where(x => x.row == plot.row + direction.row && x.column == plot.column + direction.column).Count() == 0)
                                boundaries++;
                        }
                    }
                }

                totalPrice += (region.Value.Count * boundaries);
            }
            return totalPrice;
        }

        private int CalculateSides()
        {
            int totalPrice = 0;
            foreach (var region in regions)
            {
                if (region.Value.Count == 1)
                    totalPrice += (region.Value.Count * 4);
                else
                    foreach (var plot in region.Value)
                        totalPrice += (region.Value.Count * IsACorner(plot));
            }
            return totalPrice;
        }

        private int IsACorner((char character, int row, int column) plot) =>
            combinedAngles.Sum(x => x.All(y => CheckAngle(y, plot) == true) ? 1 : 0);

        private bool CheckAngle((int row, int column, bool matchCharacter) increment, (char character, int row, int column) plot)
        {
            var outsideOfBounds = OutsideBounds(plot.row + increment.row, plot.column + increment.column);

            return increment.matchCharacter ?
                        !outsideOfBounds && map[plot.row + increment.row, plot.column + increment.column] == plot.character
                        : outsideOfBounds || map[plot.row + increment.row, plot.column + increment.column] != plot.character;
        }

        private bool OutsideBounds(int nextRow, int nextColumn) =>
            nextRow < 0 || nextRow >= map.GetLength(0) || nextColumn < 0 || nextColumn >= map.GetLength(1);
    }
}
