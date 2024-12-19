using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day16
    {
        String[] lines = File.ReadAllLines("Inputs/input.day16.txt");
        char[,] map = new char[0, 0];
        List<Reindeer> reindeers = new List<Reindeer>();
        (int row, int column) start = new();
        (int row, int column) end = new();
        Dictionary<(int row, int column), int> seen = new Dictionary<(int row, int column), int>();

        internal void ExecuteDay16Part1()
        {
            ParseInput();
            SetFirstReindeer();
            FindPaths();
            int score = CalculateBestPath();
            Console.WriteLine("Day 1, Part 1: " + score);
        }

        internal void ExecuteDay16Part2()
        {
            Console.WriteLine("Day 1, Part 2: ");
        }

        private void ParseInput()
        {
            map = new char[lines.Length, lines[0].Length];

            for (int i = 0; i < lines.Length; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];

                    if (lines[i][j] == 'S')
                        start = (i, j);

                    if (lines[i][j] == 'E')
                        end = (i, j);
                }
            }
        }

        private void SetFirstReindeer()
        {
            Reindeer reindeer = new();
            reindeer.currentLocation = start;
            reindeers.Add(reindeer);
        }

        private void FindPaths()
        {
            while (reindeers.Any(x => x.canMove))
            {
                var reindeersToMove = reindeers.Where(x => x.canMove).ToList();
                for (int i = 0; i < reindeersToMove.Count; i++)
                {
                    var reindeer = reindeersToMove[i];
                    var moves = GetMoves(reindeer);

                    foreach (var move in moves)
                    {
                        if (move.direction == reindeer.direction)
                        {
                            if (move.nextLocation == end)
                            {
                                reindeer.foundEnd = true;
                                reindeer.canMove = false;
                                break;
                            }

                            reindeer.currentLocation = move.nextLocation;
                            reindeer.canMove = move.nextCharacter == '.' && NotSeen(reindeer, move);
                            if (move.nextCharacter == '.' && NotSeen(reindeer, move))
                            {
                                reindeer.steps++;
                                seen.TryAdd(move.nextLocation, reindeer.rotations);
                            }
                        }
                        else
                        {
                            if (move.nextCharacter == '.' && NotSeen(reindeer, move))
                            {
                                Reindeer cloneReindeer = CopyReindeer(reindeer, move);
                                reindeers.Add(cloneReindeer);
                                seen.TryAdd(move.nextLocation, cloneReindeer.rotations);
                            }
                        }
                    }
                }
            }
        }

        private bool NotSeen(Reindeer reindeer, (Direction direction, char nextCharacter, (int row, int column) nextLocation) move)
        {
            return !(seen.ContainsKey(move.nextLocation) && seen[move.nextLocation] < reindeer.rotations);
        }

        private Reindeer CopyReindeer(Reindeer copy, (Direction direction, char nextCharacter, (int row, int column) nextLocation) nextLocation)
        {
            Reindeer reindeer = new Reindeer();
            reindeer.steps = copy.steps + 1;
            reindeer.direction = nextLocation.direction;
            reindeer.locations = new HashSet<(int row, int column, int steps)>(copy.locations);
            reindeer.currentLocation = nextLocation.nextLocation;
            reindeer.rotations = copy.rotations + 1;

            return reindeer;
        }

        private List<(Direction direction, char nextCharacter, (int row, int column) nextLocation)> GetMoves(Reindeer reindeer)
        {
            List<(Direction direction, char nextCharacter, (int row, int column) nextLocation)> moves = new List<(Direction direction, char nextCharacter, (int row, int column) nextLocation)>();

            switch (reindeer.direction)
            {
                case Direction.Up:
                    moves.Add((Direction.Left, map[reindeer.currentLocation.row, reindeer.currentLocation.column - 1], (reindeer.currentLocation.row, reindeer.currentLocation.column - 1)));
                    moves.Add((Direction.Right, map[reindeer.currentLocation.row, reindeer.currentLocation.column + 1], (reindeer.currentLocation.row, reindeer.currentLocation.column + 1)));
                    moves.Add((Direction.Up, map[reindeer.currentLocation.row - 1, reindeer.currentLocation.column], (reindeer.currentLocation.row - 1, reindeer.currentLocation.column)));
                    break;
                case Direction.Right:
                    moves.Add((Direction.Up, map[reindeer.currentLocation.row - 1, reindeer.currentLocation.column], (reindeer.currentLocation.row - 1, reindeer.currentLocation.column)));
                    moves.Add((Direction.Down, map[reindeer.currentLocation.row + 1, reindeer.currentLocation.column], (reindeer.currentLocation.row + 1, reindeer.currentLocation.column)));
                    moves.Add((Direction.Right, map[reindeer.currentLocation.row, reindeer.currentLocation.column + 1], (reindeer.currentLocation.row, reindeer.currentLocation.column + 1)));
                    break;
                case Direction.Down:
                    moves.Add((Direction.Right, map[reindeer.currentLocation.row, reindeer.currentLocation.column + 1], (reindeer.currentLocation.row, reindeer.currentLocation.column + 1)));
                    moves.Add((Direction.Left, map[reindeer.currentLocation.row, reindeer.currentLocation.column - 1], (reindeer.currentLocation.row, reindeer.currentLocation.column - 1)));
                    moves.Add((Direction.Down, map[reindeer.currentLocation.row + 1, reindeer.currentLocation.column], (reindeer.currentLocation.row + 1, reindeer.currentLocation.column)));
                    break;
                case Direction.Left:
                    moves.Add((Direction.Down, map[reindeer.currentLocation.row + 1, reindeer.currentLocation.column], (reindeer.currentLocation.row + 1, reindeer.currentLocation.column)));
                    moves.Add((Direction.Up, map[reindeer.currentLocation.row - 1, reindeer.currentLocation.column], (reindeer.currentLocation.row - 1, reindeer.currentLocation.column)));
                    moves.Add((Direction.Left, map[reindeer.currentLocation.row, reindeer.currentLocation.column - 1], (reindeer.currentLocation.row, reindeer.currentLocation.column - 1)));
                    break;
            }

            return moves;
        }

        private int CalculateBestPath()
        {
            var test = reindeers.Where(x => x.foundEnd).OrderBy(x => (x.rotations * 1000) + x.steps).First();
            //ColorInTheMap(test.locations.ToList());
            var pathsToAdd = reindeers.Where(x => test.locations.Contains((x.currentLocation.row, x.currentLocation.column, x.steps))).ToList();
            List<(int row, int column, int steps)> newPaths = new List<(int row, int column, int steps)>(test.locations);
            newPaths.AddRange([(start.row, start.column, 0), (end.row, end.column, 0)]);
            foreach (var path in pathsToAdd)
            {
                newPaths.AddRange(path.locations.ToList());
            }
            newPaths = newPaths.Distinct().ToList();
            var missingPathsToAdd = reindeers.Where(x => newPaths.Contains((x.currentLocation.row, x.currentLocation.column, x.steps)));
            foreach (var path in missingPathsToAdd)
            {
                newPaths.AddRange(path.locations.ToList());
            }
            newPaths = newPaths.Distinct().ToList();
            ColorInTheMap(newPaths);
            Console.WriteLine("Locations: " + newPaths.Count + ", Cost: " + (test.rotations * 1000 + test.steps - 1));
            Console.WriteLine();

            return reindeers.Where(x => x.foundEnd).Min(x => (x.rotations * 1000) + x.steps) - 1;
        }

        private void ColorInTheMap(List<(int row, int column, int steps)> joins)
        {
            foreach (var join in joins)
                map[join.row, join.column] = 'O';

            PrintTheMap(map);

            foreach (var join in joins)
                map[join.row, join.column] = '.';
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
    }

    internal class Reindeer
    {
        private (int row, int column) _currentLocation = new();
        internal HashSet<(int row, int column, int steps)> locations = new();
        internal (int row, int column) currentLocation
        {
            get { return _currentLocation; }
            set
            {
                locations.Add((value.row, value.column, this.steps));
                _currentLocation = value;
            }
        }
        internal Direction direction { get; set; } = Direction.Right;
        internal int steps { get; set; } = 0;
        internal int rotations { get; set; } = 0;
        internal bool canMove { get; set; } = true;
        internal bool foundEnd { get; set; } = false;
    }
}
