
using System.Collections.Generic;

namespace AOC2024
{
    internal class Day12
    {
        String[] lines = File.ReadAllLines("Inputs/input.day12.example.txt");
        char[,] map = new char[0, 0];
        Dictionary<int, List<(char character, int row, int column)>> regions = new Dictionary<int, List<(char character, int row, int column)>>();

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
            {
                for (int j = 0; j < lines[i].Length; j++)
                    map[i, j] = lines[i][j];
            }
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

                if (!OutsideBounds(garden.row - 1, garden.column) && map[garden.row - 1, garden.column] == garden.character)
                    newPlots.Add((garden.character, garden.row - 1, garden.column));

                if (!OutsideBounds(garden.row, garden.column + 1) && map[garden.row, garden.column + 1] == garden.character)
                    newPlots.Add((garden.character, garden.row, garden.column + 1));

                if (!OutsideBounds(garden.row + 1, garden.column) && map[garden.row + 1, garden.column] == garden.character)
                    newPlots.Add((garden.character, garden.row + 1, garden.column));

                if (!OutsideBounds(garden.row, garden.column - 1) && map[garden.row, garden.column - 1] == garden.character)
                    newPlots.Add((garden.character, garden.row, garden.column - 1));

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
                        var up = region.Value.Where(x => x.row == plot.row - 1 && x.column == plot.column).Count();
                        var right = region.Value.Where(x => x.row == plot.row && x.column == plot.column + 1).Count();
                        var down = region.Value.Where(x => x.row == plot.row + 1 && x.column == plot.column).Count();
                        var left = region.Value.Where(x => x.row == plot.row && x.column == plot.column - 1).Count();

                        if (up == 0)
                            boundaries++;
                        if (right == 0)
                            boundaries++;
                        if (down == 0)
                            boundaries++;
                        if (left == 0)
                            boundaries++;
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
                int boundaries = 0;
                var seenPlots = new List<(char character, int row, int column)>();
                if (region.Value.Count == 1)
                {
                    boundaries = 4;
                }
                else
                {
                    ContinueTheLine(region.Value[0], region.Value, Direction.Right, (region.Value[0], Direction.Right), ref boundaries, seenPlots, true);
                }

                var regionPrice = region.Value.Count * boundaries;
                totalPrice += regionPrice;
            }
            return totalPrice;
        }

        private void ContinueTheLine((char character, int row, int column) plot,
            List<(char character, int row, int column)> region,
            Direction direction,
            ((char character, int row, int column) plot, Direction direction) startingPlot,
            ref int counter,
            List<(char character, int row, int column)> seenPlots,
            bool firstPlot = false)
        {
            if (!seenPlots.Contains(plot))
                seenPlots.Add(plot);

            if (!firstPlot && plot == startingPlot.plot && direction == startingPlot.direction)
                return;

            switch (direction)
            {
                case Direction.Up:
                    var up = region.Where(x => x.row == plot.row - 1 && x.column == plot.column).ToList();

                    if (up.Count == 1)
                        ContinueTheLine(up[0], region, Direction.Up, startingPlot, ref counter, seenPlots);
                    else
                    {
                        counter += 1;
                        var upLeft = region.Where(x => x.row == plot.row && x.column == plot.column - 1).ToList();
                        if (upLeft.Count == 1)
                            ContinueTheLine(upLeft[0], region, Direction.Left, startingPlot, ref counter, seenPlots);
                        else
                            ContinueTheLine(plot, region, Direction.Right, startingPlot, ref counter, seenPlots);
                    }
                    break;
                case Direction.Right:
                    var right = region.Where(x => x.row == plot.row && x.column == plot.column + 1).ToList();

                    if (right.Count == 1)
                        ContinueTheLine(right[0], region, Direction.Right, startingPlot, ref counter, seenPlots);
                    else
                    {
                        counter += 1;
                        var rightUp = region.Where(x => x.row == plot.row - 1 && x.column == plot.column).ToList();
                        if (rightUp.Count == 1)
                            ContinueTheLine(rightUp[0], region, Direction.Up, startingPlot, ref counter, seenPlots);
                        else
                            ContinueTheLine(plot, region, Direction.Down, startingPlot, ref counter, seenPlots);
                    }
                    break;
                case Direction.Down:
                    var down = region.Where(x => x.row == plot.row + 1 && x.column == plot.column).ToList();
                    var downRight = region.Where(x => x.row == plot.row && x.column == plot.column + 1).ToList();

                    if (downRight.Count == 1)
                    {
                        counter += 1;
                        ContinueTheLine(downRight[0], region, Direction.Right, startingPlot, ref counter, seenPlots);
                    }
                    else if (down.Count == 1)
                        ContinueTheLine(down[0], region, Direction.Down, startingPlot, ref counter, seenPlots);
                    else
                    {
                        counter += 1;
                        ContinueTheLine(plot, region, Direction.Left, startingPlot, ref counter, seenPlots);
                    }
                    break;
                case Direction.Left:
                    var left = region.Where(x => x.row == plot.row && x.column == plot.column - 1).ToList();

                    if (left.Count == 1)
                        ContinueTheLine(left[0], region, Direction.Left, startingPlot, ref counter, seenPlots);
                    else
                    {
                        counter += 1;
                        var leftDown = region.Where(x => x.row == plot.row + 1 && x.column == plot.column).ToList();
                        if (leftDown.Count == 1)
                            ContinueTheLine(leftDown[0], region, Direction.Down, startingPlot, ref counter, seenPlots);
                        else
                            ContinueTheLine(plot, region, Direction.Up, startingPlot, ref counter, seenPlots);
                    }
                    break;
                default:
                    break;
            }
        }

        private bool OutsideBounds(int nextRow, int nextColumn) =>
            nextRow < 0 || nextRow >= map.GetLength(0) || nextColumn < 0 || nextColumn >= map.GetLength(1);

    }
}
