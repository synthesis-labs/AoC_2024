using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day11
    {
        String[] lines = File.ReadAllLines("Inputs/input.day11.txt");
        List<long> stones = new List<long>();
        private static Dictionary<(long, int), long> memo = new Dictionary<(long, int), long>();

        internal void ExecuteDay11Part1()
        {
            ParseInput();
            long totalStones = SolveStones(25);
            Console.WriteLine("Day 11, Part 1: " + totalStones);
        }

        internal void ExecuteDay11Part2()
        {
            long totalStones = SolveStones(75);
            Console.WriteLine("Day 11, Part 2: " + totalStones);
        }

        private void ParseInput()
        {
            foreach (var line in lines[0].Split(' '))
                stones.Add(Int32.Parse(line));
        }

        private long SolveStones(int blinks)
        {
            long totalStones = 0;
            foreach (var stone in stones)
                totalStones += CountStones(stone, blinks);

            return totalStones;
        }

        private long CountStones(long stone, int blinks)
        {
            if (blinks == 0)
                return 1;

            if (memo.ContainsKey((stone, blinks)))
                return memo[(stone, blinks)];

            long result;
            if (stone == 0)
            {
                result = CountStones(1, blinks - 1);
            }
            else if (stone.ToString().Length % 2 == 0)
            {
                string stoneString = stone.ToString();
                int left = Int32.Parse(stoneString.Substring(0, stoneString.Length / 2));
                int right = Int32.Parse(stoneString.Substring(stoneString.Length / 2));
                result = CountStones(left, blinks - 1) + CountStones(right, blinks - 1);
            }
            else
            {
                result = CountStones(stone * 2024, blinks - 1);
            }

            memo[(stone, blinks)] = result;
            return result;
        }
    }
}
