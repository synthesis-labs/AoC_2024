using System;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day7
    {
        String[] lines = File.ReadAllLines("Inputs/input.day7.txt");
        List<Calibrations> calibrations = new List<Calibrations>();
        Dictionary<(int, bool), List<Operation[]>> combinationsLookup = new Dictionary<(int, bool), List<Operation[]>>();

        internal void ExecuteDay7Part1()
        {
            ParseInput();
            PrepareCombinations();
            long sumOfValidCalibrations = TestForValidCalibrations();
            Console.WriteLine("Day 7, Part 1: " + sumOfValidCalibrations);
        }

        internal void ExecuteDay7Part2()
        {
            Stopwatch stopwatch = new Stopwatch();
            stopwatch.Start();
            long sumOfValidCalibrations = TestForValidCalibrations(false);
            stopwatch.Stop();
            Console.WriteLine("Day 7, Part 2: " + sumOfValidCalibrations + ", in: " + stopwatch.Elapsed);
        }

        private void ParseInput()
        {
            foreach (var line in lines)
            {
                var split = line.Split(':');
                var numbers = split[1].Split(' ').Where(x => x != "").Select(x => Int64.Parse(x)).ToList();
                calibrations.Add(new Calibrations() { Result = Int64.Parse(split[0]), Combinations = numbers });
            }
        }

        private void PrepareCombinations()
        {
            List<Operation> operations = Enum.GetValues(typeof(Operation)).OfType<Operation>().ToList();
            calibrations.Select(x => x.Combinations.Count - 1).Distinct().ToList().ForEach(x => 
            {
                GenerateOperatorCombinations(x, true, operations.Where(x=> x != Operation.Concatenate).ToList());
                GenerateOperatorCombinations(x, false, operations);
            });
        }

        private long TestForValidCalibrations(bool excludeConcat = true)
        {
            long sumOfValidCalibrations = 0;
            foreach (var calibration in calibrations)
            {
                List<Operation[]> combinations = combinationsLookup[(calibration.Combinations.Count - 1, excludeConcat)];

                foreach (var combination in combinations)
                {
                    long result = calibration.Combinations.First();
                    for (int i = 0; i < combination.Length; i++)
                    {
                        var operation = combination[i];
                        switch (operation)
                        {
                            case Operation.Add:
                                result += calibration.Combinations[i + 1];
                                break;
                            case Operation.Multiply:
                                result *= calibration.Combinations[i + 1];
                                break;
                            case Operation.Concatenate:
                                result = Int64.Parse(result.ToString() + calibration.Combinations[i + 1].ToString());
                                break;
                        }
                    }

                    if (calibration.Result == result)
                    {
                        sumOfValidCalibrations += calibration.Result;
                        break;
                    }
                }
            }
            return sumOfValidCalibrations;
        }

        private void GenerateOperatorCombinations(int numberOfOperators, bool excludeConcat, List<Operation> enumList)
        {
            List<Operation[]> newCombinations = new List<Operation[]>();
            void GenerateCombinations(List<Operation> currentCombination)
            {
                if (currentCombination.Count == numberOfOperators)
                {
                    newCombinations.Add(currentCombination.ToArray());
                    return;
                }

                foreach (var op in enumList)
                {
                    currentCombination.Add(op);
                    GenerateCombinations(currentCombination);
                    currentCombination.RemoveAt(currentCombination.Count - 1);
                }
            }

            GenerateCombinations(new List<Operation>());
            combinationsLookup.Add((numberOfOperators, excludeConcat), newCombinations);
        }
    }

    internal class Calibrations
    {
        internal long Result { get; set; }
        internal List<long> Combinations { get; set; } = new List<long>();
    }
}
