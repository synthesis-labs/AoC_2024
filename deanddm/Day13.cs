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

        //NB: Solution thanks to https://github.com/MartinZikmund/advent-of-code

        internal void ExecuteDay13Part1()
        {
            ParseInput();
            double totalTokens = CalculateMovesPerClaw();
            Console.WriteLine("Day 13, Part 1: " + totalTokens);
        }

        internal void ExecuteDay13Part2()
        {
            AddCrazyNumberToPrizes();
            double totalTokens = CalculateMovesPerClaw();
            Console.WriteLine("Day 13, Part 2: " + totalTokens);
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

            foreach (var claw in claws)
            {
                // Equation
                // 
                // a*ax + b*bx = prizeX
                // a*ay + b*by = prizeY
                // a = (prizeX - b*bx) / ax
                // ((prizeX - b*bx) / ax) * ay + b*by = prizeY
                // ((prizeX / ax) - (b*bx / ax)) * ay + b*by = prizeY
                // (prizeX / ax) * ay - (b*bx / ax) * ay + b*by = prizeY
                // - (b * bx / ax) * ay + b * by = prizeY - (prizeX / ax) * ay
                // b * by - (b* bx / ax) * ay = prizeY - (prizeX / ax) * ay
                // b * (by - (bx / ax) * ay) = prizeY - (prizeX / ax) * ay
                // b = (prizeY - (prizeX / ax) * ay) / (by - (bx / ax) * ay)

                long b = (long)Math.Round((claw.prizeY - (claw.prizeX / claw.buttonAX) * claw.buttonAY) / (claw.buttonBY - (claw.buttonBX / claw.buttonAX) * claw.buttonAY));
                long a = (long)Math.Round((claw.prizeX - b * claw.buttonBX) / claw.buttonAX);

                var actualX = a * claw.buttonAX + b * claw.buttonBX;
                var actualY = a * claw.buttonAY + b * claw.buttonBY;

                if (actualX == claw.prizeX &&  actualY == claw.prizeY && a >= 0 && b >= 0)
                    totalTokens += (a * 3) + b;
            }

            return totalTokens;
        }
    }

    internal class Claw
    {
        internal double buttonAX { get; set; }
        internal double buttonAY { get; set; }
        internal double buttonBX { get; set; }
        internal double buttonBY { get; set; }
        internal double prizeX { get; set; }
        internal double prizeY { get; set; }
    }
}
