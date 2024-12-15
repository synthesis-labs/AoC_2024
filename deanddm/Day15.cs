using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day15
    {
        String[] lines = File.ReadAllLines("Inputs/input.day15.txt");
        char[,] map = new char[0, 0];
        char[,] expandedMap = new char[0, 0];
        List<char> instructions = new List<char>();
        Dictionary<char, (int row, int column, int nextRow, int nextColumn, Direction direction)> directions =
            new Dictionary<char, (int row, int column, int nextRow, int nextColumn, Direction direction)>()
            {
                { '^', (-1, 0, -2, 0, Direction.Up) }, { '>', (0, 1, 0, 2, Direction.Right) },
                { 'v', (1, 0, 2, 0, Direction.Down) }, { '<', (0, -1, 0, -2, Direction.Left) }
            };
        (int row, int column) robotPosition;

        internal void ExecuteDay15Part1()
        {
            ParseInput();
            MoveTheRobot();
            int sumOfCoordinates = CalculateGPSCoordinates(map, 'O');
            Console.WriteLine("Day 15, Part 1: " + sumOfCoordinates);
        }

        internal void ExecuteDay15Part2()
        {
            instructions.Clear();
            ParseInput();
            ExpandTheMap();
            MoveTheRobotExpanded();
            int sumOfCoordinates = CalculateGPSCoordinates(expandedMap, '[');
            Console.WriteLine("Day 15, Part 2: " + sumOfCoordinates);
        }

        private void ParseInput()
        {
            int mapLines = 1;
            for (int i = 0; i < lines.Length; i++)
            {
                if (lines[i] == "")
                    break;
                mapLines++;
            }

            map = new char[mapLines - 1, lines[0].Length];
            for (int i = 0; i < mapLines; i++)
            {
                for (int j = 0; j < lines[i].Length; j++)
                {
                    map[i, j] = lines[i][j];
                    if (lines[i][j] == '@')
                        robotPosition = (i, j);
                }
            }

            for (int i = mapLines; i < lines.Length; i++)
                instructions.AddRange(lines[i]);
        }

        private void ExpandTheMap()
        {
            expandedMap = new char[map.GetLength(0) * 2, map.GetLength(1) * 2];
            List<char> expandedLines = new List<char>();

            for (int i = 0; i < map.GetLength(0); i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                {
                    var character = map[i, j];

                    if (character == '#')
                        expandedLines.AddRange(['#', '#']);
                    else if (character == 'O')
                        expandedLines.AddRange(['[', ']']);
                    else if (character == '.')
                        expandedLines.AddRange(['.', '.']);
                    else if (character == '@')
                        expandedLines.AddRange(['@', '.']);
                }

                for (int j = 0; j < expandedLines.Count; j++)
                {
                    expandedMap[i, j] = expandedLines[j];

                    if (expandedLines[j] == '@')
                        robotPosition = (i, j);
                }

                expandedLines.Clear();
            }
        }

        private void MoveTheRobot()
        {
            for (int i = 0; i < instructions.Count; i++)
            {
                var instruction = instructions[i];
                var direction = directions[instruction];
                var nextBlock = map[robotPosition.row + direction.row, robotPosition.column + direction.column];

                if (nextBlock == '#')
                    continue;

                if (nextBlock == '.')
                {
                    map[robotPosition.row, robotPosition.column] = '.';
                    robotPosition = (robotPosition.row + direction.row, robotPosition.column + direction.column);
                    map[robotPosition.row, robotPosition.column] = '@';
                    continue;
                }

                if (nextBlock == 'O')
                {
                    var searchResult = SearchTillEnd(direction, (robotPosition.row + direction.row, robotPosition.column + direction.column), map);

                    if (searchResult.searchBlock == '.')
                    {
                        map[searchResult.endRow, searchResult.endColumn] = 'O';
                        map[robotPosition.row + direction.row, robotPosition.column + direction.column] = '@';
                        map[robotPosition.row, robotPosition.column] = '.';
                        robotPosition = (robotPosition.row + direction.row, robotPosition.column + direction.column);
                        continue;
                    }
                    else if (searchResult.searchBlock == '#')
                        continue;
                }
            }
        }

        private (char searchBlock, int endRow, int endColumn) SearchTillEnd(
            (int row, int column, int nextRow, int nextColumn, Direction direction) direction,
            (int row, int column) block, char[,] map)
        {
            var continueSearch = true;
            while (continueSearch)
            {
                block = (block.row + direction.row, block.column + direction.column);
                var searchBlock = map[block.row, block.column];
                if (searchBlock == '.' || searchBlock == '#')
                    return (searchBlock, block.row, block.column);
            }

            return ('\0', 0, 0);
        }


        private void MoveTheRobotExpanded()
        {
            for (int i = 0; i < instructions.Count; i++)
            {
                var instruction = instructions[i];
                var direction = directions[instruction];
                var nextBlock = expandedMap[robotPosition.row + direction.row, robotPosition.column + direction.column];

                if (nextBlock == '#')
                    continue;

                if (nextBlock == '.')
                {
                    expandedMap[robotPosition.row, robotPosition.column] = '.';
                    robotPosition = (robotPosition.row + direction.row, robotPosition.column + direction.column);
                    expandedMap[robotPosition.row, robotPosition.column] = '@';
                    continue;
                }

                if (nextBlock == '[' || nextBlock == ']')
                {
                    if (direction.direction == Direction.Left || direction.direction == Direction.Right)
                    {
                        var searchResult = SearchTillEnd(direction, (robotPosition.row, robotPosition.column), expandedMap);

                        if (searchResult.searchBlock == '.')
                        {
                            int startColumn = direction.direction == Direction.Left ? searchResult.endColumn : robotPosition.column + direction.nextColumn;
                            int endColumn = direction.direction == Direction.Left ? robotPosition.column + direction.nextColumn : searchResult.endColumn;
                            bool isStartBox = true;
                            for (int j = startColumn; j < endColumn + 1; j++)
                            {
                                if (isStartBox)
                                    expandedMap[robotPosition.row, j] = '[';
                                else
                                    expandedMap[robotPosition.row, j] = ']';

                                isStartBox = !isStartBox;
                            }

                            expandedMap[robotPosition.row + direction.row, robotPosition.column + direction.column] = '@';
                            expandedMap[robotPosition.row, robotPosition.column] = '.';
                            robotPosition = (robotPosition.row + direction.row, robotPosition.column + direction.column);

                            continue;
                        }
                        else if (searchResult.searchBlock == '#')
                            continue;
                    }
                    else
                    {
                        var findBoxesResult = FindBoxesInDirection(direction, (robotPosition.row, robotPosition.column));

                        if (findBoxesResult.canMove)
                        {
                            findBoxesResult.boxesToMove.Reverse();
                            foreach (var box in findBoxesResult.boxesToMove)
                            {
                                for (int j = box.leftColumn; j < box.rightColumn + 1; j++)
                                {
                                    if (expandedMap[box.row, j] == '[' || expandedMap[box.row, j] == ']')
                                    {
                                        expandedMap[findBoxesResult.startRow, j] = expandedMap[box.row, j];
                                        expandedMap[box.row, j] = '.';
                                    }
                                }

                                if (direction.direction == Direction.Up)
                                    findBoxesResult.startRow++;
                                else
                                    findBoxesResult.startRow--;
                            }

                            expandedMap[robotPosition.row, robotPosition.column] = '.';
                            robotPosition = (robotPosition.row + direction.row, robotPosition.column + direction.column);
                            expandedMap[robotPosition.row, robotPosition.column] = '@';
                            continue;
                        }
                        else
                            continue;
                    }
                }
            }
        }

        private (bool canMove, int startRow, List<(int row, int leftColumn, int rightColumn)> boxesToMove) FindBoxesInDirection(
            (int row, int column, int nextRow, int nextColumn, Direction direction) direction,
            (int row, int column) block)
        {
            (int row, int leftColumn, int rightColumn) nextBox = (block.row + direction.row, block.column, block.column);
            var nextBlock = expandedMap[nextBox.row, block.column];

            if (nextBlock == ']')
                nextBox.leftColumn -= 1;
            else if (nextBlock == '[')
                nextBox.rightColumn += 1;

            List<(int row, int leftColumn, int rightColumn)> boxesToMove = new List<(int row, int leftColumn, int rightColumn)>() { nextBox };

            for (int i = 0; i < boxesToMove.Count; i++)
            {
                var box = boxesToMove[i];
                var boxRange = (row: box.row + direction.row, box.leftColumn, box.rightColumn);

                List<(char character, int column)> nextRow = new List<(char character, int column)>();
                for (int m = boxRange.leftColumn; m < boxRange.rightColumn + 1; m++)
                    nextRow.Add((expandedMap[boxRange.row, m], m));

                if (nextRow.All(x => x.character == '.'))
                    return (true, boxRange.row, boxesToMove);

                if (nextRow.Any(x => x.character == '#'))
                    return (false, 0, new());

                var firstBox = nextRow.FirstOrDefault(x => x.character == '[' || x.character == ']');
                var lastBox = nextRow.LastOrDefault(x => x.character == '[' || x.character == ']');
                boxRange.leftColumn = firstBox.column;
                boxRange.rightColumn = lastBox.column;

                var nextLeftBlock = expandedMap[boxRange.row, boxRange.leftColumn];
                var nextRightBlock = expandedMap[boxRange.row, boxRange.rightColumn];

                if (nextLeftBlock == ']')
                    boxRange.leftColumn -= 1;

                if (nextRightBlock == '[')
                    boxRange.rightColumn += 1;

                boxesToMove.Add(boxRange);
            }

            return (false, 0, new());
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

        private int CalculateGPSCoordinates(char[,] mapToUse, char characterToCheck)
        {
            int sumOfCoordinates = 0;
            for (int i = 0; i < mapToUse.GetLength(0); i++)
            {
                for (int j = 0; j < mapToUse.GetLength(1); j++)
                {
                    if (mapToUse[i, j] == characterToCheck)
                        sumOfCoordinates += (i * 100) + j;
                }
            }
            return sumOfCoordinates;
        }
    }
}
