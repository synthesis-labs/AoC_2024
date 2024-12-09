using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day9
    {
        String[] lines = File.ReadAllLines("input.day9.txt");
        List<string> initialChecksums = new List<string>();
        List<(int number, int count, string stringValue)> initialChecksumChunks = new List<(int number, int count, string stringValue)>();

        internal void ExecuteDay9Part1()
        {
            ParseInput();
            List<string> checksums = new List<string>();
            checksums.AddRange(initialChecksums);
            DefragementDisk(checksums);
            var checksum = CalculateChecksum(checksums);
            Console.WriteLine("Day 9, Part 1: " + checksum);
        }

        internal void ExecuteDay9Part2()
        {
            List<(int number, int count, string stringValue)> checksums = new List<(int number, int count, string stringValue)>();
            checksums.AddRange(initialChecksumChunks.Where(x => x.stringValue != ""));
            DefragmentWholeChunks(checksums);
            var checksum = CalculateChecksumForChunks(checksums);
            Console.WriteLine("Day 9, Part 2: " + checksum);
        }

        private void ParseInput()
        {
            var isFile = true;
            int id = 0;
            for (int i = 0; i < lines[0].Length; i++)
            {
                var character = lines[0][i];
                var number = Int32.Parse(character.ToString());
                var enumeration = Enumerable.Range(0, number);
                if (isFile)
                {
                    var value = enumeration.Select(x => id.ToString());
                    initialChecksums.AddRange(value);
                    initialChecksumChunks.Add((id, number, String.Join("", value)));
                    id++;
                }
                else
                {
                    var value = enumeration.Select(x => '.'.ToString());
                    initialChecksums.AddRange(value);
                    initialChecksumChunks.Add((0, number, String.Join("", value)));
                }

                isFile = !isFile;
            }
        }

        private void DefragementDisk(List<string> checksums)
        {
            for (int i = 0; i < checksums.Count; i++)
            {
                if (Int32.TryParse(checksums[i], out int number))
                    continue;

                if (checksums.Skip(i + 1).All(x => x == "."))
                    break;

                var endNumber = checksums.Last(x => x != ".");
                int endIndex = checksums.LastIndexOf(endNumber);

                checksums.RemoveAt(endIndex);
                checksums.Insert(i, endNumber);
                checksums.RemoveAt(i + 1);
                checksums.Insert(endIndex, ".");
            }
        }

        //00992111777.44.333....5555.6666.....8888..
        private void DefragmentWholeChunks(List<(int number, int count, string stringValue)> checksums)
        {
            checksums.Reverse();

            for (int i = 0; i < checksums.Count; i++)
            {
                var checksum = checksums[i];

                if (checksum.stringValue.StartsWith('.'))
                    continue;

                var endNumber = checksums.LastOrDefault(x => x.stringValue.StartsWith('.') && x.count >= checksum.count);

                if (endNumber.stringValue != "")
                {
                    int endIndex = checksums.LastIndexOf(endNumber);

                    if (endIndex < i)
                        continue;

                    checksums[endIndex] = checksum;

                    int remainder = endNumber.count - checksum.count;
                    if (remainder > 0)
                    {
                        checksums.Insert(endIndex, (0, remainder, String.Join("", Enumerable.Range(0, remainder).Select(x => '.'))));
                    }

                    checksums[i] = (0, checksum.count, String.Join("", Enumerable.Range(0, checksum.count).Select(x => '.')));

                }
            }

            checksums.Reverse();
        }

        private long CalculateChecksum(List<string> checksums)
        {
            long checksum = 0;

            for (int i = 0; i < checksums.Count; ++i)
            {
                if (checksums[i].StartsWith("."))
                    break;

                checksum += (i * Int32.Parse(checksums[i]));
            }

            return checksum;
        }

        private long CalculateChecksumForChunks(List<(int number, int count, string stringValue)> checksums)
        {
            long calculation = 0;

            int id = 0;
            foreach (var checksum in  checksums)
            {
                if (checksum.number == 0)
                {
                    id += checksum.stringValue.Length;
                    continue;
                }

                foreach (var iteration in checksum.stringValue.Chunk(checksum.number.ToString().Length))
                {
                    calculation += (id * checksum.number);
                    id++;
                }
            }

            return calculation;
        }
    }
}
