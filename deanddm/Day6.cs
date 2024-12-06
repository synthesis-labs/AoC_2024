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
        int startingRow, startingColumn, loopCount = 0;
        HashSet<(int row, int column)> uniqueLocations = new HashSet<(int, int)>();

        internal void ExecuteDay6Part1()
        {
            ParseInput();
            WalkTheGuard();
            Console.WriteLine("Day 6, Part 1: " + uniqueLocations.Count);
        }

        internal void ExecuteDay6Part2()
        {
            Stopwatch sw = Stopwatch.StartNew();
            PlaceObstaclesAndWalkTheGuard();
            sw.Stop();
            Console.WriteLine("Day 6, Part 2: " + loopCount + ", Time: " + sw.Elapsed);
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];
            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];
                    if (lines[i][j] == '^')
                    {
                        startingRow = i;
                        startingColumn = j;
                        uniqueLocations.Add((startingRow, startingColumn));
                    }
                }
            }
        }

        private bool WalkTheGuard((int, int)? obstacle = null)
        {
            (int row, int column, Direction direction) coordinate = (startingRow, startingColumn, Direction.Up);
            var visitedLocations = new HashSet<(int, int, Direction)>();

            while (true)
            {
                coordinate = MoveNextPosition(coordinate, obstacle);

                if (!visitedLocations.Add(coordinate))
                    return false;

                if (coordinate.direction == Direction.Out)
                    return true;

                if (obstacle == null)
                {
                    if (!uniqueLocations.Contains((coordinate.row, coordinate.column)))
                        uniqueLocations.Add((coordinate.row, coordinate.column));
                }
            }
        }

        private void PlaceObstaclesAndWalkTheGuard()
        {
            var validLocations = uniqueLocations.Where(location => location != (startingRow, startingColumn) && map[location.row, location.column] == '.');
            Parallel.ForEach(validLocations, new ParallelOptions() { MaxDegreeOfParallelism = Environment.ProcessorCount }, (obstacle) =>
            {
                if (!WalkTheGuard(obstacle))
                    Interlocked.Increment(ref loopCount);
            });
        }

        private (int row, int column, Direction direction) MoveNextPosition((int row, int column, Direction direction) coordinate, (int row, int column)? obstacle)
        {
            (int nextRow, int nextColumn) = coordinate.direction switch
            {
                Direction.Up => (coordinate.row - 1, coordinate.column),
                Direction.Down => (coordinate.row + 1, coordinate.column),
                Direction.Left => (coordinate.row, coordinate.column - 1),
                Direction.Right => (coordinate.row, coordinate.column + 1),
                _ => throw new Exception("Not sure where to go")
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

        private bool OutsideBounds(int nextRow, int nextColumn) =>
            nextRow < 0 || nextRow >= map.GetLength(0) || nextColumn < 0 || nextColumn >= map.GetLength(1);

        private (int row, int column, Direction newDirection) Rotate90Degrees(int nextRow, int nextColumn, Direction direction) =>
            direction switch
            {
                Direction.Up => (nextRow + 1, nextColumn, Direction.Right),
                Direction.Down => (nextRow - 1, nextColumn, Direction.Left),
                Direction.Left => (nextRow, nextColumn + 1, Direction.Up),
                Direction.Right => (nextRow, nextColumn - 1, Direction.Down),
                _ => throw new Exception("Not sure where to turn")
            };
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
