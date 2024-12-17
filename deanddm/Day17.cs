using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day17
    {
        String[] lines = File.ReadAllLines("Inputs/input.day17.txt");
        List<double> instructions = new List<double>();

        internal void ExecuteDay17Part1()
        {
            double registerA = ParseInput();
            List<double> output = RunProgram(registerA);
            Console.WriteLine("Day 17, Part 1: " + String.Join(',', output));
        }

        internal void ExecuteDay17Part2()
        {
            var answer = SolveForInstructions(instructions, 0);
            Console.WriteLine("Day 17, Part 2: " + answer);
        }

        private double ParseInput()
        {
            var instruction = lines[4].Split("Program: ", StringSplitOptions.RemoveEmptyEntries)[0].Split(',', StringSplitOptions.RemoveEmptyEntries);
            instructions.AddRange(instruction.Select(x => double.Parse(x)));

            return double.Parse(lines[0].Split("Register A: ", StringSplitOptions.RemoveEmptyEntries)[0]);
        }

        private List<double> RunProgram(double registerA)
        {
            List<double> output = new List<double>();
            double registerB = 0;
            double registerC = 0;

            for (int i = 0; i < instructions.Count; i++)
            {
                if (i + 1 >= instructions.Count)
                    break;

                var opsCode = instructions[i];
                var operand = instructions[i + 1];
                double combo;

                switch (operand)
                {
                    case 4:
                        combo = registerA;
                        break;
                    case 5:
                        combo = registerB;
                        break;
                    case 6:
                        combo = registerC;
                        break;
                    case 7:
                        throw new Exception("Invalid program");
                    default:
                        combo = operand;
                        break;
                }

                switch (opsCode)
                {
                    case 0:
                        var zero = registerA / (Math.Pow(2, combo));
                        registerA = double.Truncate(zero);
                        break;
                    case 1:
                        var one = Convert.ToInt64(registerB) ^ Convert.ToInt64(operand);
                        registerB = one;
                        break;
                    case 2:
                        var two = combo % 8;
                        registerB = two;
                        break;
                    case 3:
                        if (registerA != 0)
                        {
                            i = Convert.ToInt32(operand) - 1;
                            continue;
                        }
                        break;
                    case 4:
                        var four = Convert.ToInt64(registerB) ^ Convert.ToInt64(registerC);
                        registerB = four;
                        break;
                    case 5:
                        var five = combo % 8;
                        output.Add(five);
                        break;
                    case 6:
                        var six = registerA / (Math.Pow(2, combo));
                        registerB = double.Truncate(six);
                        break;
                    case 7:
                        var seven = registerA / (Math.Pow(2, combo));
                        registerC = double.Truncate(seven);
                        break;
                }

                i++;
            }

            return output;
        }

        private double? SolveForInstructions(List<double> nextInstructions, double answer)
        {
            if (nextInstructions.Count <= 0)
                return answer;

            for (long i = 0; i < 8; i++)
            {
                double registerA = Convert.ToInt64(answer) * 8 | i;
                double registerB = 0;
                double registerC = 0;

                double? output = null;

                for (int j = 0; j < instructions.Count; j++)
                {
                    var opsCode = instructions[j];
                    var operand = instructions[j + 1];
                    double combo;

                    switch (operand)
                    {
                        case 4:
                            combo = registerA;
                            break;
                        case 5:
                            combo = registerB;
                            break;
                        case 6:
                            combo = registerC;
                            break;
                        case 7:
                            throw new Exception("Invalid program");
                        default:
                            combo = operand;
                            break;
                    }

                    switch (opsCode)
                    {
                        case 0:
                            //var zero = registerA / (Math.Pow(2, combo));
                            //registerA = double.Truncate(zero);
                            break;
                        case 1:
                            var one = Convert.ToInt64(registerB) ^ Convert.ToInt64(operand);
                            registerB = one;
                            break;
                        case 2:
                            var two = combo % 8;
                            registerB = two;
                            break;
                        case 3:
                            break;
                        case 4:
                            var four = Convert.ToInt64(registerB) ^ Convert.ToInt64(registerC);
                            registerB = four;
                            break;
                        case 5:
                            var five = combo % 8;
                            output = five;
                            break;
                        case 6:
                            var six = registerA / (Math.Pow(2, combo));
                            registerB = double.Truncate(six);
                            break;
                        case 7:
                            var seven = registerA / (Math.Pow(2, combo));
                            registerC = double.Truncate(seven);
                            break;
                    }

                    j++;

                    if (output == nextInstructions.Last())
                    {
                        var next = SolveForInstructions(nextInstructions.Take(nextInstructions.Count - 1).ToList(), Convert.ToInt64(registerA) * 8 | i);

                        if (next == null)
                            continue;
                        else
                            return next;
                    }
                }
            }

            return answer;
        }
    }
}
