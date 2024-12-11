using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day10
    {
        String[] lines = File.ReadAllLines("Inputs/input.day10.txt");
        char[,] map = new char[0, 0];
        List<(int row, int column, int number)> startingLocations = new List<(int row, int column, int number)>();
        List<Direction> directions = new List<Direction>() { Direction.Up, Direction.Right, Direction.Down, Direction.Left };
        int score, rating = 0;

        internal void ExecuteDay10Part1()
        {
            ParseInput();
            SearchTheTrails();
            Console.WriteLine("Day 10, Part 1: " + score);
        }

        internal void ExecuteDay10Part2()
        {
            Console.WriteLine("Day 10, Part 2: " + rating);
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];
            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];

                    if (map[i, j] == '0')
                        startingLocations.Add((i, j, 0));
                }
            }
        }

        private void SearchTheTrails()
        {
            foreach (var startingLocation in startingLocations)
            {
                List<(int row, int column, int number)> locations = new List<(int row, int column, int number)>() { startingLocation };
                for (int i = 1; i < 10; i++)
                {
                    locations = NavigateNext(locations, i);
                }
                score += locations.Distinct().Count();
                rating += locations.Count;
            }
        }

        private List<(int row, int column, int number)> NavigateNext(List<(int row, int column, int number)> locations, int nextNumber)
        {
            List<(int row, int column, int number)> nextLocations = new List<(int row, int column, int number)>();
            foreach (var location in locations) 
            {
                foreach (var direction in directions)
                {
                    var nextLocation = NavigateInDirection(location, direction, nextNumber);
                    if (nextLocation.number != -1)
                        nextLocations.Add(nextLocation);
                }
            }
            return nextLocations;
        }

        private (int row, int column, int number) NavigateInDirection((int row, int column, int number) location, Direction direction, int nextNumber)
        {
            var nextDirection = GetDirection(location, direction);

            if (OutsideBounds(nextDirection.row, nextDirection.column) || 
                map[nextDirection.row, nextDirection.column] == '.' || 
                Int32.Parse(map[nextDirection.row, nextDirection.column].ToString()) != nextNumber)
                return (-1, -1, -1);

            return (nextDirection.row, nextDirection.column, nextNumber);
        }

        private (int row, int column) GetDirection((int row, int column, int number) location, Direction direction) =>
            direction switch
            {
                Direction.Up => (location.row - 1, location.column),
                Direction.Right => (location.row, location.column + 1),
                Direction.Down => (location.row + 1, location.column),
                Direction.Left => (location.row, location.column - 1),
                _ => throw new Exception("Not sure where to move")
            };

        private bool OutsideBounds(int nextRow, int nextColumn) =>
            nextRow < 0 || nextRow >= map.GetLength(0) || nextColumn < 0 || nextColumn >= map.GetLength(1);

    }
}
