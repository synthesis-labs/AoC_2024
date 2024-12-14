using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day14
    {
        String[] lines = File.ReadAllLines("Inputs/input.day14.txt");
        List<Robot> robots = new List<Robot>();
        int x = 101; // 11 / 101
        int y = 103; // 7 / 103
        char[,] map = new char[0, 0];

        internal void ExecuteDay14Part1()
        {
            ParseInput();
            int safetyFactor = MoveRobotsForIterations(100);
            Console.WriteLine("Day 14, Part 1: " + safetyFactor);
        }

        internal void ExecuteDay14Part2()
        {
            robots.Clear();
            ParseInput();
            map = new char[x, y];
            int secondsToFindTree = FindTheTree(100000);
            Console.WriteLine("Day 14, Part 2: " + secondsToFindTree);
        }

        private void ParseInput()
        {
            foreach (var line in lines)
            {
                var robot = new Robot();
                var positionSplit = line.Split(' ');

                var position = positionSplit[0].Split("p=", StringSplitOptions.RemoveEmptyEntries)[0].Split(',');
                robot.startPositionX = Int32.Parse(position[0]);
                robot.startPositionY = Int32.Parse(position[1]);

                var velocity = positionSplit[1].Split("v=", StringSplitOptions.RemoveEmptyEntries)[0].Split(',');
                robot.velocityX = Int32.Parse(velocity[0]);
                robot.velocityY = Int32.Parse(velocity[1]);

                robots.Add(robot);
            }
        }

        private int MoveRobotsForIterations(int iterations)
        {
            for (int i = 0; i < iterations; i++)
                robots.ForEach(robot => robot.MoveOne(x, y));

            int halfX = (x - 1) / 2;
            int halfY = (y - 1) / 2;
            var quadrant1 = robots.Where(robot => robot.startPositionX >= 0 && robot.startPositionX < halfX && robot.startPositionY >= 0 && robot.startPositionY < halfY).ToList();
            var quadrant2 = robots.Where(robot => robot.startPositionX >= (halfX + 1) && robot.startPositionX < x && robot.startPositionY >= 0 && robot.startPositionY < halfY).ToList();
            var quadrant3 = robots.Where(robot => robot.startPositionX >= 0 && robot.startPositionX < halfX && robot.startPositionY >= (halfY + 1) && robot.startPositionY < y).ToList();
            var quadrant4 = robots.Where(robot => robot.startPositionX >= (halfX + 1) && robot.startPositionX < x && robot.startPositionY >= (halfY + 1) && robot.startPositionY < y).ToList();

            return quadrant1.Count * quadrant2.Count * quadrant3.Count * quadrant4.Count;
        }

        private int FindTheTree(int iterations)
        {
            for (int i = 0; i < iterations; i++)
            {
                robots.ForEach(robot => robot.MoveOne(x, y));
                var foundTheTree = PrintChristmasTree(i + 1);

                if (foundTheTree)
                    return i + 1;
            }

            return 0;
        }

        private bool PrintChristmasTree(int seconds)
        {
            ResetMap();
            foreach (var robot in robots)
            {
                if (map[robot.startPositionX, robot.startPositionY] == ' ')
                    map[robot.startPositionX, robot.startPositionY] = 'X';
            }
            bool foundTheTree = false;
            for (int i = 0; i < map.GetLength(0); i++)
            {
                int xCount = 0;
                for (int j = 0; j < map.GetLength(1); j++)
                {
                    if (map[i, j] == 'X' && (j > 0 && map[i, j - 1] == 'X'))
                        xCount++;
                }

                if (xCount > 20)
                {
                    foundTheTree = true;
                    break;
                }
            }

            //PrintMap();
            return foundTheTree;

        }

        private void ResetMap()
        {
            for (int i = 0; i < map.GetLength(0); i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                    map[i, j] = ' ';
            }
        }

        private void PrintMap()
        {
            //Console.WriteLine();
            //Console.WriteLine();
            //Console.WriteLine("Seconds: " + seconds);

            for (int i = 0; i < map.GetLength(0); i++)
            {
                for (int j = 0; j < map.GetLength(1); j++)
                    Console.Write(map[i, j]);

                Console.WriteLine();
            }

            //Console.WriteLine("Seconds: " + seconds);
            //Console.WriteLine();
            //Console.WriteLine();
        }
    }

    internal class Robot
    {
        internal int startPositionX { get; set; }
        internal int startPositionY { get; set; }
        internal int velocityX { get; set; }
        internal int velocityY { get; set; }

        internal void MoveOne(int x, int y)
        {
            startPositionX += velocityX;
            if (startPositionX < 0)
                startPositionX = x - Math.Abs(startPositionX);
            else if (startPositionX > (x - 1))
                startPositionX = startPositionX - x;
            startPositionY += velocityY;
            if (startPositionY < 0)
                startPositionY = y - Math.Abs(startPositionY);
            else if (startPositionY > (y - 1))
                startPositionY = startPositionY - y;
        }
    }
}