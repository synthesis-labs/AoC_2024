using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day2
    {
        String[] lines = File.ReadAllLines("input.day2.txt");
        List<List<int>> reports = new List<List<int>>();

        internal void ExecuteDay2Part1()
        {
            ParseInput();
            int safeLevels = CalculateSafeLevels();

            Console.WriteLine("Day 2, Part 1: " + safeLevels);
        }

        internal void ExecuteDay2Part2()
        {
            int safeLevels = CalculateSafeLevelsByRemovingALevel();
            Console.WriteLine("Day 2, Part 2: " + safeLevels);
        }

        private void ParseInput()
        {
            reports = lines.Select(x => x.Split(' ').Select(y => Int32.Parse(y)).ToList()).ToList();
        }

        private int CalculateSafeLevels()
        {
            int safeLevels = 0;
            foreach (var report in reports)
            {
                if (CheckIfSafe(report))
                    safeLevels++;
            }

            return safeLevels;
        }

        private int CalculateSafeLevelsByRemovingALevel()
        {
            int safeLevels = 0;
            foreach (var report in reports)
            {
                if (CheckIfSafe(report))
                {
                    safeLevels++;
                    continue;
                }

                for (int i = 0; i < report.Count; i++)
                {
                    var modifiedReport = new List<int>(report);
                    modifiedReport.RemoveAt(i);
                    if (CheckIfSafe(modifiedReport))
                    {
                        safeLevels++;
                        break;
                    }
                }
            }

            return safeLevels;
        }

        private bool CheckIfSafe(List<int> report)
        {
            var previousLevel = 0;
            var increase = true;

            if (report[0] - report[report.Count - 1] > 0)
                increase = false;

            foreach (var level in report)
            {
                if (previousLevel == 0)
                {
                    previousLevel = level;
                    continue;
                }

                if (previousLevel == level)
                    return false;

                if (increase && level - previousLevel > 0 && level - previousLevel < 4) { } // Safe
                else if (!increase && previousLevel - level > 0 && previousLevel - level < 4) { } // Safe
                else // Unsafe
                    return false;

                previousLevel = level;
            }

            return true;
        }
    }
}
