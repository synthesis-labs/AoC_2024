using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day13
    {
        String[] lines = File.ReadAllLines("Inputs/input.day13.txt");
        List<Claw> claws = new List<Claw>();

        internal void ExecuteDay13Part1()
        {
            ParseInput();
            double totalTokens = CalculateMovesPerClaw();
            Console.WriteLine("Day 13, Part 1: " + totalTokens);
        }

        internal void ExecuteDay13Part2()
        {
            //AddCrazyNumberToPrizes();
            double totalTokens = CalculateMovesPerClaw();
            Console.WriteLine("Day 13, Part 2: ");
        }

        private void ParseInput()
        {
            Claw claw = new Claw();
            foreach (var line in lines)
            {
                if (line == "")
                    continue;

                if (line.StartsWith("Button A"))
                {
                    var values = line.Replace("Button A: ", "").Replace("X+", "").Replace("Y+", "").Split(", ");
                    claw.buttonAX = Int32.Parse(values[0]);
                    claw.buttonAY = Int32.Parse(values[1]);
                }
                else if (line.StartsWith("Button B"))
                {
                    var values = line.Replace("Button B: ", "").Replace("X+", "").Replace("Y+", "").Split(", ");
                    claw.buttonBX = Int32.Parse(values[0]);
                    claw.buttonBY = Int32.Parse(values[1]);
                }
                else
                {
                    var values = line.Replace("Prize: ", "").Replace("X=", "").Replace("Y=", "").Split(", ");
                    claw.prizeX = Int32.Parse(values[0]);
                    claw.prizeY = Int32.Parse(values[1]);

                    claws.Add(claw);
                    claw = new Claw();
                }
            }
        }

        private void AddCrazyNumberToPrizes()
        {
            foreach (var claw in claws)
            {
                claw.prizeX += 10000000000000;
                claw.prizeY += 10000000000000;
            }
        }

        private double CalculateMovesPerClaw()
        {
            double totalTokens = 0;
            List<(double buttonAPresses, double buttonBPresses, double cost)> presses = new List<(double buttonAPresses, double buttonBPresses, double cost)>();

            foreach (var claw in claws)
            {
                double amountOfButtonAX = claw.prizeX / claw.buttonAX;
                double amountOfButtonBX = claw.prizeX / claw.buttonBX;
                double currentX, currentY = 0;
                bool exitX = false;
                bool exitY = false;

                for (double i = 1; i < amountOfButtonAX; i++)
                {
                    if (exitX)
                        break;

                    for (double j = 1; j < amountOfButtonBX; j++)
                    {
                        currentX = (claw.buttonAX * i) + (claw.buttonBX * j);
                        currentY = (claw.buttonAY * i) + (claw.buttonBY * j);

                        if (claw.prizeX == currentX &&  claw.prizeY == currentY)
                        {
                            presses.Add((i, j, (i * 3) + j));
                            exitX = true;
                            break;
                        }
                    }
                }

                double amountOfButtonAY = claw.prizeY / claw.buttonAY;
                double amountOfButtonBY = claw.prizeY / claw.buttonBY;

                for (double i = 1; i < amountOfButtonAY; i++)
                {
                    if (exitY)
                        break;

                    for (double j = 1; j < amountOfButtonBY; j++)
                    {
                        currentX = (claw.buttonAX * i) + (claw.buttonBX * j);
                        currentY = (claw.buttonAY * i) + (claw.buttonBY * j);

                        if (claw.prizeX == currentX && claw.prizeY == currentY)
                        {
                            presses.Add((i, j, (i * 3) + j));
                            exitY = true;
                            break;
                        }
                    }
                }

                var minTokens = presses.OrderBy(x => x.cost).FirstOrDefault();
                totalTokens += minTokens.cost;
                presses.Clear();
            }

            return totalTokens;
        }
    }

    internal class Claw
    {
        internal int buttonAX { get; set; }
        internal int buttonAY { get; set; }
        internal int buttonBX { get; set; }
        internal int buttonBY { get; set; }
        internal double prizeX { get; set; }
        internal double prizeY { get; set; }
    }
}
