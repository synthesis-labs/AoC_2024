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
        String[] lines = File.ReadAllLines("input.day3.txt");
        int total = 0;

        internal void ExecuteDay3Part1()
        {
            ParseInput();

            Console.WriteLine("Day 3, Part 1: " + total);
        }

        private void ParseInput()
        {
            bool doAction = true;
            Match? commandMatch = null;
            foreach (var line in lines)
            {
                // Regular expression to match "mul(number,number)"
                string pattern = @"mul\(\d+,\d+\)";

                // Create a regex object
                Regex regex = new Regex(pattern);

                // Find all matches
                MatchCollection matches = regex.Matches(line);

                string commandPattern = @"do(?:n't)?\(\)";
                Regex commandRegex = new Regex(commandPattern);
                MatchCollection commandMatches = commandRegex.Matches(line);

                // Output each valid "mul(number,number)" match
                Console.WriteLine("Valid instructions:");
                foreach (Match match in matches)
                {
                    commandMatch = commandMatches.LastOrDefault(x => x.Index < match.Index);

                    if (commandMatch != null)
                        doAction = commandMatch.Value == "do()";

                    if (doAction)
                    {
                        var numberStrings = match.Value.Replace("mul(", "").Replace(")", "").Split(',');
                        total += Int32.Parse(numberStrings[0]) * Int32.Parse(numberStrings[1]);
                    }
                }
            }
        }
    }
}
