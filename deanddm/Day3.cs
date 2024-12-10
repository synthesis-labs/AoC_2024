using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day3
    {
        String[] lines = File.ReadAllLines("Inputs/input.day3.txt");
        Regex combinedRegex = new Regex(@"mul\(\d+,\d+\)|do(?:n't)?\(\)");

        internal void ExecuteDay3Part1()
        {
            int multiplication = PerformMultiplicationWithCommands(true);
            Console.WriteLine("Day 3, Part 1: " + multiplication);
        }

        internal void ExecuteDay3Part2()
        {
            int multiplicationWithCommands = PerformMultiplicationWithCommands();
            Console.WriteLine("Day 3, Part 2: " + multiplicationWithCommands);
        }

        private int PerformMultiplicationWithCommands(bool excludeCommand = false)
        {
            bool doAction = true;
            return lines
                .Select(x => combinedRegex.Matches(x))
                .Sum(x => x.Aggregate(0, (total, next) =>
                {
                    if (next.Value == "do()" || next.Value == "don't()")
                    {
                        doAction = next.Value == "do()";
                        return total;
                    }

                    return total += (excludeCommand || doAction) ?
                        next.Value
                        .Replace("mul(", "")
                        .Replace(")", "")
                        .Split(',')
                        .Select(y => Int32.Parse(y))
                            .ToList()
                            .Aggregate(1, (mulTotal, mulNext) => mulTotal * mulNext)
                        : 0;
                }
                ));
        }
    }
}
