using System;
using System.Collections.Generic;
using System.Diagnostics.Metrics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day19
    {
        String[] lines = File.ReadAllLines("Inputs/input.day19.txt");
        List<string> patterns = new List<string>();
        List<string> designs = new List<string>();
        List<string> solvedDesigns = new List<string>();
        Dictionary<char, List<string>> combinations = new Dictionary<char, List<string>>();

        internal void ExecuteDay19Part1()
        {
            ParseInput();
            int canSolve = FindPossibleDesigns();
            Console.WriteLine("Day 19, Part 1: " + canSolve);
        }

        internal void ExecuteDay19Part2()
        {
            PopulateCombinations();
            long allCombinations = FindAllPossibleDesigns();
            Console.WriteLine("Day 19, Part 2: " + allCombinations);
        }

        private void ParseInput()
        {
            patterns.AddRange(lines[0].Split(", ", StringSplitOptions.RemoveEmptyEntries));

            for (int i = 2; i < lines.Length; i++)
                designs.Add(lines[i]);
        }

        private int FindPossibleDesigns()
        {
            int canSolve = 0;
            foreach (var design in designs)
            {
                if (IteratePatterns(design).canSolve)
                {
                    solvedDesigns.Add(design);
                    canSolve++;
                }
            }
            return canSolve;
        }

        private (bool canSolve, string remainder) IteratePatterns(string remainder)
        {
            foreach (var pattern in patterns)
            {
                if (remainder.StartsWith(pattern))
                {
                    var (canSolve, iterateRemainder) = IteratePatterns(remainder.Replace(pattern, ""));

                    if (canSolve)
                        remainder = iterateRemainder;
                }
            }
            return (remainder.Length == 0, remainder);
        }

        private void PopulateCombinations()
        {
            foreach (var pattern in patterns)
            {
                if (!combinations.ContainsKey(pattern[0]))
                    combinations.Add(pattern[0], new List<string>());

                combinations[pattern[0]].Add(pattern);
            }
        }

        private long FindAllPossibleDesigns()
        {
            long solvedDesignCount = 0;

            foreach (var design in solvedDesigns)
            {
                long[] indexes = new long[design.Length + 1];
                indexes[0] = 1;

                for (int i = 0; i < design.Length; i++)
                {
                    char nextChar = design[i];
                    if (combinations.ContainsKey(nextChar) && combinations[nextChar].Count > 0)
                    {
                        foreach (var pattern in combinations[nextChar])
                        {
                            int nextIndex = i + pattern.Length;

                            if (nextIndex <= design.Length && design.Substring(i, pattern.Length) == pattern)
                                indexes[nextIndex] += indexes[i];
                        }
                    }
                }

                solvedDesignCount += indexes[design.Length];
            }

            return solvedDesignCount;
        }
    }
}
