using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day1
    {
        String[] lines = File.ReadAllLines("Inputs/input.day1.txt");
        List<int> teamA = new List<int>();
        List<int> teamB = new List<int>();

        internal void ExecuteDay1Part1()
        {
            ParseInput();
            int distances = CalculateDistances();
            Console.WriteLine("Day 1, Part 1: " + distances);
        }

        internal void ExecuteDay1Part2()
        {
            int similarities = CalculateSimilarities();
            Console.WriteLine("Day 1, Part 2: " + similarities);
        }

        private void ParseInput()
        {
            var splitLines = lines.Select(x => x.Split("   ")).Select(x => x.Select(y => Int32.Parse(y)).ToList()).ToList();
            splitLines.ForEach(x =>
            {
                teamA.Add(x[0]);
                teamB.Add(x[1]);
            });
            teamA = teamA.OrderBy(x => x).ToList();
            teamB = teamB.OrderBy(x => x).ToList();
        }

        private int CalculateDistances()
        {
            return teamA.Zip(teamB).Aggregate(0, (total, next) => total += Math.Abs(next.Second - next.First));
        }

        private int CalculateSimilarities()
        {
            return teamA.Aggregate(0, (total, next) => total += teamB.Where(b => b == next).Count() * next);
        }
    }
}
