using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day6
    {
        String[] lines = File.ReadAllLines("input.day6.txt");
        char[,] map = new char[0, 0];
        int startingRow = 0;
        int startingColumn = 0;
        Dictionary<(int, int), int> startingUniqueLocations = new Dictionary<(int, int), int>();
        int loops = 0;
        int maxLocationVisits = 0;

        internal void ExecuteDay6Part1()
        {
            ParseInput();
            WalkTheGuard(startingUniqueLocations, null);
            maxLocationVisits = startingUniqueLocations.Max(x => x.Value) + 1;
            Console.WriteLine("Day 6, Part 1: " + startingUniqueLocations.Count);
        }

        internal void ExecuteDay6Part2()
        {
            Stopwatch sw = Stopwatch.StartNew();
            PlaceObstaclesAndWalkTheGuard();
            sw.Stop();
            Console.WriteLine("Day 6, Part 2: " + loops + ", in: " + sw.Elapsed);
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];

            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    var character = lines[i][j];
                    map[i, j] = character;

                    if (character == '^')
                    {
                        startingRow = i;
                        startingColumn = j;
                    }
                }
            }

            startingUniqueLocations.Add((startingRow, startingColumn), 1);
        }

        private bool WalkTheGuard(Dictionary<(int, int), int> uniqueLocations, (int, int)? obstacle)
        {
            bool doneWalking = false;
            (int row, int column, Direction direction) coordinate = (startingRow, startingColumn, Direction.Up);
            
            while (!doneWalking)
            {
                coordinate = MoveNextPosition(coordinate, obstacle);

                if (coordinate.direction == Direction.Out)
                    doneWalking = true;
                else
                {
                    if (!uniqueLocations.ContainsKey((coordinate.row, coordinate.column)))
                        uniqueLocations.Add((coordinate.row, coordinate.column), 1);
                    else
                        uniqueLocations[(coordinate.row, coordinate.column)]++;
                }

                if (obstacle != null && uniqueLocations.Any(x=> x.Value > maxLocationVisits))
                    return false;
            }

            return true;
        }

        private void PlaceObstaclesAndWalkTheGuard()
        {
            Parallel.ForEach(startingUniqueLocations, new ParallelOptions() { MaxDegreeOfParallelism = Environment.ProcessorCount }, (uniqueLocation) =>
            {
                var (row, column) = uniqueLocation.Key;
                if (row == startingRow && column == startingColumn)
                    return;

                var character = map[row, column];
                if (character == '.')
                {
                    Dictionary<(int, int), int> newUniqueLocations = new Dictionary<(int, int), int>() { { (startingRow, startingColumn), 1 } };
                    var exitedTheCourse = WalkTheGuard(newUniqueLocations, uniqueLocation.Key);

                    if (!exitedTheCourse)
                        loops++;
                }
            });
        }

        private (int row, int column, Direction direction) MoveNextPosition((int row, int column, Direction direction) coordinate, (int row, int column)? obstacle)
        {
            (int nextRow, int nextColumn) = coordinate.direction switch
            {
                Direction.Up => (coordinate.row - 1, coordinate.column),
                Direction.Down => (coordinate.row + 1, coordinate.column),
                Direction.Left => (coordinate.row, coordinate.column - 1),
                Direction.Right => (coordinate.row, coordinate.column + 1)
            };

            if (OutsideBounds(nextRow, nextColumn))
                return (0, 0, Direction.Out);

            char nextCharacter = map[nextRow, nextColumn];
            bool isAnObstacle = IsAnObstacle(obstacle, nextRow, nextColumn);

            if ((nextCharacter == '.' || nextCharacter == '^') && !isAnObstacle)
                return (nextRow, nextColumn, coordinate.direction);

            if (nextCharacter == '#' || isAnObstacle)
                return Rotate90Degrees(nextRow, nextColumn, coordinate.direction);

            throw new Exception("Not sure what to do");
        }

        private bool IsAnObstacle((int row, int column)? obstacle, int row, int column)
        {
            return obstacle.HasValue && obstacle.Value.row == row && obstacle.Value.column == column;
        }

        private bool OutsideBounds(int nextRow, int nextColumn)
        {
            if (nextRow < 0 || nextRow >= map.GetLength(0) || nextColumn < 0 || nextColumn >= map.GetLength(1))
                return true;
            return false;
        }

        private (int row, int column, Direction newDirection) Rotate90Degrees(int nextRow, int nextColumn, Direction direction)
        {
            return direction switch
            {
                Direction.Up => (nextRow + 1, nextColumn, Direction.Right),
                Direction.Down => (nextRow - 1, nextColumn, Direction.Left),
                Direction.Left => (nextRow, nextColumn + 1, Direction.Up), 
                Direction.Right => (nextRow, nextColumn - 1, Direction.Down)
            };
        }
    }

    internal enum Direction
    {
        Up,
        Down,
        Left,
        Right,
        Out
    }
}
