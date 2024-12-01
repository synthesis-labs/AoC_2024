﻿/**
 * This utility namespace is largely based on:
 * https://github.com/jeroenheijmans/advent-of-code-2018/blob/master/AdventOfCode2018/Util.cs
 */

using System.Collections;
using System.Globalization;
using System.Numerics;
using System.Text;
using System.Text.RegularExpressions;

namespace Utilities
{
    public enum CompassDirection
    {
        N = 0,
        NE = 45,
        E = 90,
        SE = 135,
        S = 180,
        SW = 225,
        W = 270,
        NW = 315
    }

    public static class MainUtilities
    {
        private const CompassDirection N = CompassDirection.N;
        private const CompassDirection S = CompassDirection.S;
        private const CompassDirection E = CompassDirection.E;
        private const CompassDirection W = CompassDirection.W;
        private const CompassDirection NE = CompassDirection.NE;
        private const CompassDirection NW = CompassDirection.NW;
        private const CompassDirection SE = CompassDirection.SE;
        private const CompassDirection SW = CompassDirection.SW;
        public static CompassDirection Flip(this CompassDirection dir)
        {
            return (dir) switch
            {
                N => S,
                S => N,
                E => W,
                W => E,
                NE => SW,
                SW => NE,
                SE => NW,
                NW => SE,
                _ => throw new ArgumentException()
            };
        }

        /// <summary>
        /// Turns a string into a list of ints. 
        /// </summary>
        /// <param name="str">The string to split up</param>
        /// <param name="delimiter">How to split. Default is each character becomes an int.</param>
        /// <returns>A list of integers</returns>
        public static List<int> ToIntList(this string str, string delimiter = "")
        {
            if (delimiter == "")
            {
                List<int> result = new();
                foreach (char c in str) if (int.TryParse(c.ToString(), out int n)) result.Add(n);
                return result;
            }
            else
            {
                return str
                    .Split(delimiter)
                    .Where(n => int.TryParse(n, out int v))
                    .Select(n => Convert.ToInt32(n))
                    .ToList();
            }
        }

        /// <summary>
        /// Turns a string array into an array of int. 
        /// Utilizes ToIntList
        /// </summary>
        /// <param name="array">array of strings to parse</param>
        /// <returns>An array of integers</returns>
        public static int[] ToIntArray(this string[] array)
        {
            return string.Join(",", array).ToIntList(",").ToArray();
        }

        /// <summary>
        /// Extract all the positive integers from a string, automatically deliminates on all non numeric chars
        /// </summary>
        /// <param name="str">String to search</param>
        /// <returns>An ordered enumerable of the integers found in the string.</returns>
        public static IEnumerable<int> ExtractPosInts(this string str)
        {
            return Regex.Matches(str, "\\d+").Select(m => int.Parse(m.Value));
        }

        /// <summary>
        /// Extracts all ints from a string, treats `-` as a negative sign.
        /// </summary>
        /// <param name="str">String to search</param>
        /// <returns>An ordered enumerable of the integers found in the string.</returns>
        public static IEnumerable<int> ExtractInts(this string str)
        {
            return Regex.Matches(str, "-?\\d+").Select(m => int.Parse(m.Value));
        }

        /// <summary>
        /// Extracts all ints from a string, treats `-` as a negative sign.
        /// </summary>
        /// <param name="str">String to search</param>
        /// <returns>An ordered enumerable of the integers found in the string.</returns>
        public static IEnumerable<long> ExtractLongs(this string str)
        {
            return Regex.Matches(str, "-?\\d+").Select(m => long.Parse(m.Value));
        }

        /// <summary>
        /// Extract all the positive integers from a string, automatically deliminates on all non numeric chars
        /// </summary>
        /// <param name="str">String to search</param>
        /// <returns>An ordered enumerable of the integers found in the string.</returns>
        public static IEnumerable<long> ExtractPosLongs(this string str)
        {
            return Regex.Matches(str, "\\d+").Select(m => long.Parse(m.Value));
        }

        /// <summary>
        /// Extracts all "Words" (including xnoppyt) from a string
        /// </summary>
        /// <param name="str"></param>
        /// <returns></returns>
        public static IEnumerable<string> ExtractWords(this string str)
        {
            return Regex.Matches(str, "[a-zA-z]+").Select(a => a.Value);
        }

        /// <summary>
        /// Turns a string into a list of longs. 
        /// </summary>
        /// <param name="str">The string to split up</param>
        /// <param name="delimiter">How to split. Default is each character becomes a long.</param>
        /// <returns>A list of long</returns>
        public static List<long> ToLongList(this string str, string delimiter = "")
        {
            if (delimiter == "")
            {
                List<long> result = new();
                foreach (char c in str) if (long.TryParse(c.ToString(), out long n)) result.Add(n);
                return result.ToList();
            }
            else
            {
                return str
                    .Split(delimiter)
                    .Where(n => long.TryParse(n, out long v))
                    .Select(n => Convert.ToInt64(n))
                    .ToList();
            }
        }

        /// <summary>
        /// Repeat text n times, returning a string with the repeated text.
        /// </summary>
        /// <param name="text">Text to Repeat.</param>
        /// <param name="n">Number of times to repeat.</param>
        /// <param name="seperator"></param>
        /// <returns>Repeated String.</returns>
        public static string Repeat(this string text, int n, string seperator = "")
        {
            return new StringBuilder((text.Length + seperator.Length) * n)
              .Insert(0, $"{text}{seperator}", n)
              .ToString();
        }

        public static IEnumerable<T> Flatten<T>(this T[,] map)
        {
            for (int row = 0; row < map.GetLength(0); row++)
            {
                for (int col = 0; col < map.GetLength(1); col++)
                {
                    yield return map[row, col];
                }
            }
        }

        public static string JoinAsStrings<T>(this IEnumerable<T> items, char seperator = '\u0000')
        {
            return string.Join(seperator, items);
        }

        public static string JoinAsStrings<T>(this IEnumerable<T> items, string seperator)
        {
            return string.Join(seperator, items);
        }

        public static List<string> SplitByNewline(this string input, bool blankLines = false, bool shouldTrim = true)
        {
            return input
               .Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.None)
               .Where(s => blankLines || !string.IsNullOrWhiteSpace(s))
               .Select(s => shouldTrim ? s.Trim() : s)
               .ToList();
        }
        public static List<string> SplitByDoubleNewline(this string input, bool blankLines = false, bool shouldTrim = true)
        {
            return input
               .Split(new[] { "\r\n\r\n", "\r\r", "\n\n" }, StringSplitOptions.None)
               .Where(s => blankLines || !string.IsNullOrWhiteSpace(s))
               .Select(s => shouldTrim ? s.Trim() : s)
               .ToList();
        }

        /// <summary>
        /// Splits the input into columns, this is sometimes nice for maps drawing. 
        /// Automatically expands to a full rectangle if needed based on max length and number of rows. 
        /// Empty cells are denoted as ' ' (Space character)
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
        public static string[] SplitIntoColumns(this string input)
        {
            var rows = input.SplitByNewline(false, false);
            int numColumns = rows.Max(x => x.Length);

            var res = new string[numColumns];
            for (int i = 0; i < numColumns; i++)
            {
                StringBuilder sb = new();
                foreach (var row in rows)
                {
                    try
                    {
                        sb.Append(row[i]);
                    }
                    catch (IndexOutOfRangeException)
                    {
                        sb.Append(' ');
                    }
                }
                res[i] = sb.ToString();
            }
            return res;
        }

        //Removes a specified row AND column from a multidimensional array.
        public static int[,] TrimArray(this int[,] originalArray, int rowToRemove, int columnToRemove)
        {
            int[,] result = new int[originalArray.GetLength(0) - 1, originalArray.GetLength(1) - 1];

            for (int i = 0, j = 0; i < originalArray.GetLength(0); i++)
            {
                if (i == rowToRemove)
                    continue;

                for (int k = 0, u = 0; k < originalArray.GetLength(1); k++)
                {
                    if (k == columnToRemove)
                        continue;

                    result[j, u] = originalArray[i, k];
                    u++;
                }
                j++;
            }

            return result;
        }

        public static int ManhattanDistance(this (int x, int y) a, (int x, int y) b)
        {
            return Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y);
        }

        public static int ManhattanDistance(this (int x, int y, int z) a, (int x, int y, int z) b)
        {
            return Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y) + Math.Abs(a.z - b.z);
        }

        public static long ManhattanDistance(this (long x, long y) a, (long x, long y) b)
        {
            return Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y);
        }

        public static long ManhattanDistance(this (long x, long y, long z) a, (long x, long y, long z) b)
        {
            return Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y) + Math.Abs(a.z - b.z);
        }

        public static long ManhattanMagnitude(this (long x, long y, long z) a) => a.ManhattanDistance((0, 0, 0));

        public static double FindGCD(double a, double b)
        {
            if (a == 0 || b == 0) return Math.Max(a, b);
            return (a % b == 0) ? b : FindGCD(b, a % b);
        }

        public static double FindLCM(double a, double b) => a * b / FindGCD(a, b);

        public static long FindGCD(long a, long b)
        {
            if (a == 0 || b == 0) return Math.Max(a, b);
            return (a % b == 0) ? b : FindGCD(b, a % b);
        }
        public static long FindLCM(long a, long b) => a * b / FindGCD(a, b);

        public static (long gcd, long x, long y) ExtendedGCD(long a, long b)
        {
            if (b == 0) return (a, 1, 0);
            var (gcd0, x0, y0) = ExtendedGCD(b, b % a);
            return (gcd0, y0, x0 - (a / b) * y0);
        }

        public static int Mod(int x, int m)
        {
            int r = x % m;
            return r < 0 ? r + m : r;
        }

        public static long Mod(long x, long m)
        {
            long r = x % m;
            return r < 0 ? r + m : r;
        }

        public static long ModInverse(long a, long n)
        {
            return ModPower(a, n - 2, n);
        }

        public static long ModPower(long x, long y, long p)
        {
            return (long)BigInteger.ModPow(x, y, p);
        }

        public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values)
        {
            return (values.Count() == 1) ? new[] { values } : values.SelectMany(v => Permutations(values.Where(x => x.Equals(v) == false)), (v, p) => p.Prepend(v)).ToList();
        }

        public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values, int subcount)
        {
            var comboList = Combinations(values, subcount).ToList();
            foreach (IEnumerable<T> combination in comboList)
            {
                IEnumerable<IEnumerable<T>> perms = Permutations(combination);
                foreach (int i in Enumerable.Range(0, perms.Count())) yield return perms.ElementAt(i);
            }
        }

        // Enumerate all possible m-size combinations of [0, 1, ..., n-1] array
        // in lexicographic order (first [0, 1, 2, ..., m-1]).
        private static IEnumerable<int[]> Combinations(int m, int n)
        {
            int[] result = new int[m];
            Stack<int> stack = new(m);
            stack.Push(0);
            while (stack.Count > 0)
            {
                int index = stack.Count - 1;
                int value = stack.Pop();
                while (value < n)
                {
                    result[index++] = value++;
                    stack.Push(value);
                    if (index != m) continue;
                    yield return (int[])result.Clone();
                    break;
                }
            }
        }

        public static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> array, int m)
        {
            if (array.Count() < m)
                throw new ArgumentException("Array length can't be less than number of selected elements");
            if (m < 1)
                throw new ArgumentException("Number of selected elements can't be less than 1");
            T[] result = new T[m];
            foreach (int[] j in Combinations(m, array.Count()))
            {
                for (int i = 0; i < m; i++)
                {
                    result[i] = array.ElementAt(j[i]);
                }
                yield return result;
            }
        }

        public static IEnumerable<IEnumerable<T>> Split<T>(this IEnumerable<T> array, int size)
        {
            for (int i = 0; i < (float)array.Count() / size; i++)
            {
                yield return array.Skip(i * size).Take(size);
            }
        }

        public static IEnumerable<List<T>> SplitAtIndex<T>(this List<T> array, int index)
        {
            if (index == 0) throw new ArgumentException($"{nameof(index)} must be a non-zero integer");
            else if (index > 0)
            {
                index %= array.Count;
                yield return array.Take(index).ToList();
                yield return array.Skip(index).ToList();

            }
            else
            {
                index *= -1;
                index %= array.Count;
                yield return array.SkipLast(index).ToList();
                yield return array.TakeLast(index).ToList();
            }
        }

        public static string[] ToStringArray(this char[][] array)
        {
            var tmp = new string[array.GetLength(0)];

            for (int i = 0; i < tmp.Length; i++)
            {
                tmp[i] = array[i].JoinAsStrings();
            }
            return tmp;
        }

        /// <summary>
        /// Rotates an IEnumerable by the requested amount
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="array"></param>
        /// <param name="rotations">Number of steps to take, positive numbers move indices down (first item moves to end of array), negative numbers move them up (item at end moves to start)</param>
        /// <returns></returns>
        public static IEnumerable<T> Rotate<T>(this IEnumerable<T> array, int rotations)
        {
            for (int i = 0; i < array.Count(); i++)
            {
                yield return i + rotations >= 0 ? array.ElementAt((i + rotations) % array.Count()) : array.ElementAt((i + rotations) + array.Count());
            }
        }

        public static (int x, int y) Add(this (int x, int y) a, (int x, int y) b) => (a.x + b.x, a.y + b.y);

        public static (int x, int y, int z) Add(this (int x, int y, int z) a, (int x, int y, int z) b) => (a.x + b.x, a.y + b.y, a.z + b.z);

        public static (int x, int y, int z, int w) Add(this (int x, int y, int z, int w) a, (int x, int y, int z, int w) b) => (a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);

        public static (long x, long y) Add(this (long x, long y) a, (long x, long y) b) => (a.x + b.x, a.y + b.y);

        public static (long x, long y, long z) Add(this (long x, long y, long z) a, (long x, long y, long z) b) => (a.x + b.x, a.y + b.y, a.z + b.z);

        public static (long x, long y, long z, long w) Add(this (long x, long y, long z, long w) a, (long x, long y, long z, long w) b) => (a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);

        public static IEnumerable<int> AllIndexesOf(this string str, string value)
        {
            if (string.IsNullOrEmpty(value))
                throw new ArgumentException("the string to find may not be empty", nameof(value));
            for (int index = 0; ; index += value.Length)
            {
                index = str.IndexOf(value, index);
                if (index == -1)
                    break;
                yield return index;
            }
        }

        public static string HexStringToBinary(this string Hexstring)
        {
            return string.Join(string.Empty, Hexstring.Select(c => Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0')));
        }

        public static (int x, int y) MoveDirection(this (int, int) start, CompassDirection Direction, bool flipY = false, int distance = 1)
        {
            if (flipY)
            {
                return (Direction) switch
                {
                    N => start.Add((0, -distance)),
                    NE => start.Add((distance, -distance)),
                    E => start.Add((distance, 0)),
                    SE => start.Add((distance, distance)),
                    S => start.Add((0, distance)),
                    SW => start.Add((-distance, distance)),
                    W => start.Add((-distance, 0)),
                    NW => start.Add((-distance, -distance)),
                    _ => throw new ArgumentException("Direction is not valid", nameof(Direction))
                };
            }
            else
            {
                return (Direction) switch
                {
                    N => start.Add((0, distance)),
                    NE => start.Add((distance, distance)),
                    E => start.Add((distance, 0)),
                    SE => start.Add((distance, -distance)),
                    S => start.Add((0, -distance)),
                    SW => start.Add((-distance, -distance)),
                    W => start.Add((-distance, 0)),
                    NW => start.Add((-distance, distance)),
                    _ => throw new ArgumentException("Direction is not valid", nameof(Direction))
                };
            }
        }

        public static Coordinate2D MoveDirection(this Coordinate2D start, CompassDirection Direction, bool flipY = false, int distance = 1)
        {
            if (flipY)
            {
                return (Direction) switch
                {
                    N => start + (0, -distance),
                    NE => start + (distance, -distance),
                    E => start + (distance, 0),
                    SE => start + (distance, distance),
                    S => start + (0, distance),
                    SW => start + (-distance, distance),
                    W => start + (-distance, 0),
                    NW => start + (-distance, -distance),
                    _ => throw new ArgumentException("Direction is not valid", nameof(Direction))
                };
            }
            else
            {
                return (Direction) switch
                {
                    N => start + (0, distance),
                    NE => start + (distance, distance),
                    E => start + (distance, 0),
                    SE => start + (distance, -distance),
                    S => start + (0, -distance),
                    SW => start + (-distance, -distance),
                    W => start + (-distance, 0),
                    NW => start + (-distance, distance),
                    _ => throw new ArgumentException("Direction is not valid", nameof(Direction))
                };
            }
        }


        public static CompassDirection Turn(this CompassDirection value, string turnDir, int degrees = 90)
        {
            return (turnDir.ToLower()) switch
            {
                "l" or "ccw" => (CompassDirection)(((int)value - degrees + 360) % 360),
                "r" or "cw" => (CompassDirection)(((int)value + degrees) % 360),
                _ => throw new ArgumentException("Value must be L, R, CCW, or CW", nameof(turnDir)),
            };
        }


        public static T GetDirection<T>(this Dictionary<(int, int), T> values, (int, int) location, CompassDirection Direction, T defaultVal)
        {
            var n = location.MoveDirection(Direction);
            return values.GetValueOrDefault(n, defaultVal);
        }

        public static T GetDirection<T>(this Dictionary<Coordinate2D, T> values, Coordinate2D location, CompassDirection Direction, T defaultVal)
        {
            var n = location.MoveDirection(Direction);
            return values.GetValueOrDefault(n, defaultVal);
        }

        public static List<T> Get2dNeighborVals<T>(this Dictionary<(int, int), T> values, (int, int) location, T defaultVal, bool includeDiagonals = false)
        {
            List<T> res =
            [
                values.GetDirection(location, N, defaultVal),
                values.GetDirection(location, E, defaultVal),
                values.GetDirection(location, S, defaultVal),
                values.GetDirection(location, W, defaultVal)
            ];

            if (includeDiagonals)
            {
                res.Add(values.GetDirection(location, NW, defaultVal));
                res.Add(values.GetDirection(location, NE, defaultVal));
                res.Add(values.GetDirection(location, SE, defaultVal));
                res.Add(values.GetDirection(location, SW, defaultVal));
            }


            return res;
        }

        public static List<T> Get2dNeighborVals<T>(this Dictionary<Coordinate2D, T> values, Coordinate2D location, T defaultVal, bool includeDiagonals = false)
        {
            List<T> res = new()
            {
                values.GetDirection(location, N, defaultVal),
                values.GetDirection(location, E, defaultVal),
                values.GetDirection(location, S, defaultVal),
                values.GetDirection(location, W, defaultVal)
            };

            if (includeDiagonals)
            {
                res.Add(values.GetDirection(location, NW, defaultVal));
                res.Add(values.GetDirection(location, NE, defaultVal));
                res.Add(values.GetDirection(location, SE, defaultVal));
                res.Add(values.GetDirection(location, SW, defaultVal));
            }

            return res;
        }

        public static List<K> KeyList<K, V>(this Dictionary<K, V> dictionary, bool sorted = false)
        {
            List<K> keyList = [.. dictionary.Keys];

            if (sorted) keyList.Sort();

            return keyList;
        }

        public static List<Coordinate2D> Neighbors(this Coordinate2D val, bool includeDiagonals = false)
        {
            var tmp = new List<Coordinate2D>()
            {
                new(val.x - 1, val.y),
                new(val.x + 1, val.y),
                new(val.x, val.y - 1),
                new(val.x, val.y + 1),
            };
            if (includeDiagonals)
            {
                tmp.AddRange(new List<Coordinate2D>()
                {
                    new(val.x - 1, val.y - 1),
                    new(val.x + 1, val.y - 1),
                    new(val.x - 1, val.y + 1),
                    new(val.x + 1, val.y + 1),
                });
            }
            return tmp;
        }

        public static IEnumerable<Coordinate3D> GetImmediateNeighbors(this Coordinate3D self)
        {
            yield return (self.x + 1, self.y, self.z);
            yield return (self.x - 1, self.y, self.z);
            yield return (self.x, self.y + 1, self.z);
            yield return (self.x, self.y - 1, self.z);
            yield return (self.x, self.y, self.z + 1);
            yield return (self.x, self.y, self.z - 1);
        }

        public static List<Coordinate2D> AStar(Coordinate2D start, Coordinate2D goal, Dictionary<Coordinate2D, long> map, out long Cost, bool IncludeDiagonals = false, bool IncludePath = true)
        {
            PriorityQueue<Coordinate2D, long> openSet = new();
            Dictionary<Coordinate2D, Coordinate2D> cameFrom = new();

            Dictionary<Coordinate2D, long> gScore = new()
            {
                [start] = 0
            };

            openSet.Enqueue(start, 0);

            while (openSet.TryDequeue(out Coordinate2D cur, out long _))
            {
                if (cur.Equals(goal))
                {
                    Cost = gScore[cur];
                    return IncludePath ? ReconstructPath(cameFrom, cur) : null;
                }

                foreach (var n in cur.Neighbors(IncludeDiagonals).Where(a => map.ContainsKey(a)))
                {
                    var tentGScore = gScore[cur] + map[n];
                    if (tentGScore < gScore.GetValueOrDefault(n, int.MaxValue))
                    {
                        cameFrom[n] = cur;
                        gScore[n] = tentGScore;
                        openSet.Enqueue(n, tentGScore + cur.ManDistance(goal));
                    }
                }
            }

            Cost = long.MaxValue;
            return null;
        }

        private static List<Coordinate2D> ReconstructPath(Dictionary<Coordinate2D, Coordinate2D> cameFrom, Coordinate2D current)
        {
            List<Coordinate2D> res = new()
            {
                current
            };
            while (cameFrom.ContainsKey(current))
            {
                current = cameFrom[current];
                res.Add(current);
            }
            res.Reverse();
            return res;
        }

        public static (Dictionary<Coordinate2D, char> map, int maxX, int maxY) GenerateMap(this string self, bool discardDot = true)
        {
            var lines = self.SplitByNewline();
            int maxX = 0;
            int maxY = lines.Count - 1;
            Dictionary<Coordinate2D, char> res = new();

            for (int i = 0; i < lines.Count; i++)
            {
                maxX = Math.Max(maxX, lines[i].Length - 1);
                for (int j = 0; j < lines[i].Length; j++)
                {
                    if (!discardDot || lines[i][j] != '.')
                    {
                        res[(j, i)] = lines[i][j];
                    }
                }
            }

            return (res, maxX, maxY);
        }

        public static string StringFromMap<TValue>(this Dictionary<Coordinate2D, TValue> self, int maxX, int maxY, bool AssumeEmptyIsDot = true)
        {
            StringBuilder sb = new();
            for (int y = 0; y <= maxY; y++)
            {
                for (int x = 0; x <= maxX; x++)
                {
                    if (self.TryGetValue((x, y), out TValue val))
                    {
                        sb.Append(val);
                    }
                    else if (AssumeEmptyIsDot)
                    {
                        sb.Append(".");
                    }
                    else
                    {
                        sb.Append(string.Empty);
                    }
                }
                sb.Append("\n");
            }

            return sb.ToString();
        }
        public static IEnumerable<string> Lines(this string str)
        {
            using var sr = new StringReader(str ?? string.Empty);

            while (true)
            {
                var line = sr.ReadLine();
                if (line == null)
                    yield break;
                yield return line;
            }
        }

        public static Dictionary<Point2D<int>, char> Cells(this string str, Func<char, bool> filter = null) => str.Cells(c => c, filter);

        public static Dictionary<Point2D<int>, T> Cells<T>(this string str, Func<char, T> selector, Func<char, bool> filter = null)
            => str.Lines()
                .SelectMany((l, y) => l.Select((c, x) => (x, y, c)))
                .Where(n => filter?.Invoke(n.c) ?? true)
                .ToDictionary(n => new Point2D<int>(n.x, n.y), n => selector(n.c));

        public static StringMap<T> AsMap<T>(this string str, Func<char, T> selector)
            => new StringMap<T>(str, selector);

        public static StringMap<char> AsMap(this string str)
            => new StringMap<char>(str, c => c);
    }

    #region Coordinates
    public class Coordinate2D
    {
        public static readonly Coordinate2D origin = new(0, 0);
        public static readonly Coordinate2D unit_x = new(1, 0);
        public static readonly Coordinate2D unit_y = new(0, 1);
        public readonly int x;
        public readonly int y;

        public Coordinate2D(int x, int y)
        {
            this.x = x;
            this.y = y;
        }

        public Coordinate2D((int x, int y) coord)
        {
            this.x = coord.x;
            this.y = coord.y;
        }

        public Coordinate2D(string StringPair)
        {
            var t = StringPair.Split(',');
            x = int.Parse(t[0]);
            y = int.Parse(t[1]);
        }

        public Coordinate2D RotateCW(int degrees, Coordinate2D center)
        {
            Coordinate2D offset = center - this;
            return center + offset.RotateCW(degrees);
        }
        public Coordinate2D RotateCW(int degrees)
        {
            return ((degrees / 90) % 4) switch
            {
                0 => this,
                1 => RotateCW(),
                2 => -this,
                3 => RotateCCW(),
                _ => this,
            };
        }

        private Coordinate2D RotateCW()
        {
            return new Coordinate2D(y, -x);
        }

        public Coordinate2D RotateCCW(int degrees, Coordinate2D center)
        {
            Coordinate2D offset = center - this;
            return center + offset.RotateCCW(degrees);
        }
        public Coordinate2D RotateCCW(int degrees)
        {
            return ((degrees / 90) % 4) switch
            {
                0 => this,
                1 => RotateCCW(),
                2 => -this,
                3 => RotateCW(),
                _ => this,
            };
        }

        private Coordinate2D RotateCCW()
        {
            return new Coordinate2D(-y, x);
        }

        public static Coordinate2D operator +(Coordinate2D a) => a;
        public static Coordinate2D operator +(Coordinate2D a, Coordinate2D b) => new(a.x + b.x, a.y + b.y);
        public static Coordinate2D operator -(Coordinate2D a) => new(-a.x, -a.y);
        public static Coordinate2D operator -(Coordinate2D a, Coordinate2D b) => a + (-b);
        public static Coordinate2D operator *(int scale, Coordinate2D a) => new(scale * a.x, scale * a.y);
        public static bool operator ==(Coordinate2D a, Coordinate2D b) => (a.x == b.x && a.y == b.y);
        public static bool operator !=(Coordinate2D a, Coordinate2D b) => (a.x != b.x || a.y != b.y);

        public static implicit operator Coordinate2D((int x, int y) a) => new(a.x, a.y);

        public static implicit operator (int x, int y)(Coordinate2D a) => (a.x, a.y);

        public int ManDistance(Coordinate2D other)
        {
            int x = Math.Abs(this.x - other.x);
            int y = Math.Abs(this.y - other.y);
            return x + y;
        }

        public override bool Equals(object obj)
        {
            if (obj == null) return false;
            if (obj.GetType() != typeof(Coordinate2D)) return false;
            return this == (Coordinate2D)obj;
        }

        public override int GetHashCode()
        {
            return (100 * x + y).GetHashCode();
        }

        public override string ToString()
        {
            return string.Concat("(", x, ", ", y, ")");
        }
        public void Deconstruct(out int xVal, out int yVal)
        {
            xVal = x;
            yVal = y;
        }

    }


    public class Coordinate2DL
    {
        public static readonly Coordinate2DL origin = new(0, 0);
        public static readonly Coordinate2DL unit_x = new(1, 0);
        public static readonly Coordinate2DL unit_y = new(0, 1);
        public readonly long x;
        public readonly long y;

        public Coordinate2DL(long x, long y)
        {
            this.x = x;
            this.y = y;
        }

        public Coordinate2DL((long x, long y) coord)
        {
            this.x = coord.x;
            this.y = coord.y;
        }

        public Coordinate2DL RotateCW(int degrees, Coordinate2DL center)
        {
            Coordinate2DL offset = center - this;
            return center + offset.RotateCW(degrees);
        }
        public Coordinate2DL RotateCW(int degrees)
        {
            return ((degrees / 90) % 4) switch
            {
                0 => this,
                1 => RotateCW(),
                2 => -this,
                3 => RotateCCW(),
                _ => this,
            };
        }

        private Coordinate2DL RotateCW()
        {
            return new Coordinate2DL(y, -x);
        }

        public Coordinate2DL RotateCCW(int degrees, Coordinate2DL center)
        {
            Coordinate2DL offset = center - this;
            return center + offset.RotateCCW(degrees);
        }
        public Coordinate2DL RotateCCW(int degrees)
        {
            return ((degrees / 90) % 4) switch
            {
                0 => this,
                1 => RotateCCW(),
                2 => -this,
                3 => RotateCW(),
                _ => this,
            };
        }

        private Coordinate2DL RotateCCW()
        {
            return new Coordinate2DL(-y, x);
        }

        public static Coordinate2DL operator +(Coordinate2DL a) => a;
        public static Coordinate2DL operator +(Coordinate2DL a, Coordinate2DL b) => new(a.x + b.x, a.y + b.y);
        public static Coordinate2DL operator -(Coordinate2DL a) => new(-a.x, -a.y);
        public static Coordinate2DL operator -(Coordinate2DL a, Coordinate2DL b) => a + (-b);
        public static Coordinate2DL operator *(long scale, Coordinate2DL a) => new(scale * a.x, scale * a.y);
        public static bool operator ==(Coordinate2DL a, Coordinate2DL b) => (a.x == b.x && a.y == b.y);
        public static bool operator !=(Coordinate2DL a, Coordinate2DL b) => (a.x != b.x || a.y != b.y);

        public static implicit operator Coordinate2DL((long x, long y) a) => new(a.x, a.y);

        public static implicit operator (long x, long y)(Coordinate2DL a) => (a.x, a.y);

        public long ManDistance(Coordinate2DL other)
        {
            long x = Math.Abs(this.x - other.x);
            long y = Math.Abs(this.y - other.y);
            return x + y;
        }
        public override bool Equals(object obj)
        {
            if (obj == null) return false;
            if (obj.GetType() != typeof(Coordinate2DL)) return false;
            return this == (Coordinate2DL)obj;
        }

        public override int GetHashCode()
        {
            return (100 * x + y).GetHashCode();
        }

        public override string ToString()
        {
            return string.Concat("(", x, ", ", y, ")");
        }

        public void Deconstruct(out long xVal, out long yVal)
        {
            xVal = x;
            yVal = y;
        }

    }

    public class Coordinate3D
    {
        public readonly int x;
        public readonly int y;
        public readonly int z;

        public Coordinate3D(int x, int y, int z)
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public Coordinate3D(string stringformat)
        {
            var n = stringformat.ExtractInts().ToArray();
            x = n[0];
            y = n[1];
            z = n[2];
        }

        public static implicit operator Coordinate3D((int x, int y, int z) a) => new(a.x, a.y, a.z);

        public static implicit operator (int x, int y, int z)(Coordinate3D a) => (a.x, a.y, a.z);
        public static Coordinate3D operator +(Coordinate3D a) => a;
        public static Coordinate3D operator +(Coordinate3D a, Coordinate3D b) => new(a.x + b.x, a.y + b.y, a.z + b.z);
        public static Coordinate3D operator -(Coordinate3D a) => new(-a.x, -a.y, -a.z);
        public static Coordinate3D operator -(Coordinate3D a, Coordinate3D b) => a + (-b);
        public static bool operator ==(Coordinate3D a, Coordinate3D b) => (a.x == b.x && a.y == b.y && a.z == b.z);
        public static bool operator !=(Coordinate3D a, Coordinate3D b) => (a.x != b.x || a.y != b.y || a.z != b.z);

        public int ManhattanDistance(Coordinate3D other) => (int)(Math.Abs(x - other.x) + Math.Abs(y - other.y) + Math.Abs(z - other.z));
        public int ManhattanMagnitude() => Math.Abs(x) + Math.Abs(y) + Math.Abs(z);

        public override bool Equals(object obj)
        {
            if (obj.GetType() != typeof(Coordinate3D)) return false;
            return this == (Coordinate3D)obj;
        }

        public override int GetHashCode()
        {
            //Primes times coordinates for fewer collisions
            return (137 * x + 149 * y + 163 * z);
        }
        public override string ToString()
        {
            return $"{x}, {y}, {z}";
        }

        public static Coordinate3D[] GetNeighbors()
        {
            return neighbors3D;
        }

        internal void Deconstruct(out int x, out int y, out int z)
        {
            x = this.x;
            y = this.y;
            z = this.z;
        }

        private static readonly Coordinate3D[] neighbors3D =
        {
            (-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),
            (0,-1,-1), (0,-1,0), (0,-1,1), (0,0,-1),          (0,0,1), (0,1,-1), (0,1,0), (0,1,1),
            (1,-1,-1), (1,-1,0), (1,-1,1), (1,0,-1), (1,0,0), (1,0,1), (1,1,-1), (1,1,0), (1,1,1)
        };

        public List<Coordinate3D> Rotations => new()
        {
            (x,y,z),
            (x,z,-y),
            (x,-y,-z),
            (x,-z,y),
            (y,x,-z),
            (y,z,x),
            (y,-x,z),
            (y,-z,-x),
            (z,x,y),
            (z,y,-x),
            (z,-x,-y),
            (z,-y,x),
            (-x,y,-z),
            (-x,z,y),
            (-x,-y,z),
            (-x,-z,-y),
            (-y,x,z),
            (-y,z,-x),
            (-y,-x,-z),
            (-y,-z,x),
            (-z,x,-y),
            (-z,y,x),
            (-z,-x,y),
            (-z,-y,-x)
        };
    }



    public class Coordinate4D
    {
        readonly int x;
        readonly int y;
        readonly int z;
        readonly int w;

        public Coordinate4D(int x, int y, int z, int w)
        {
            this.x = x;
            this.y = y;
            this.z = z;
            this.w = w;
        }

        public static implicit operator Coordinate4D((int x, int y, int z, int w) a) => new(a.x, a.y, a.z, a.w);

        public static implicit operator (int x, int y, int z, int w)(Coordinate4D a) => (a.x, a.y, a.z, a.w);
        public static Coordinate4D operator +(Coordinate4D a) => a;
        public static Coordinate4D operator +(Coordinate4D a, Coordinate4D b) => new(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w);
        public static Coordinate4D operator -(Coordinate4D a) => new(-a.x, -a.y, -a.z, -a.w);
        public static Coordinate4D operator -(Coordinate4D a, Coordinate4D b) => a + (-b);
        public static bool operator ==(Coordinate4D a, Coordinate4D b) => (a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w);
        public static bool operator !=(Coordinate4D a, Coordinate4D b) => (a.x != b.x || a.y != b.y || a.z != b.z || a.z != b.z);

        public int ManhattanDistance(Coordinate4D other) => (int)(Math.Abs(x - other.x) + Math.Abs(y - other.y) + Math.Abs(z - other.z) + Math.Abs(w - other.w));

        public override bool Equals(object obj)
        {
            if (obj.GetType() != typeof(Coordinate4D)) return false;
            return this == (Coordinate4D)obj;
        }

        public override int GetHashCode()
        {
            return (137 * x + 149 * y + 163 * z + 179 * w);
        }


        public static Coordinate4D[] GetNeighbors()
        {
            if (neighbors != null) return neighbors;

            List<Coordinate4D> neighborList = new();

            for (int x = -1; x <= 1; x++)
            {
                for (int y = -1; y <= 1; y++)
                {
                    for (int z = -1; z <= 1; z++)
                    {
                        for (int w = -1; w <= 1; w++)
                        {
                            if (!((0 == x) && (0 == y) && (0 == z) && (0 == w)))
                            {
                                neighborList.Add((x, y, z, w));
                            }
                        }
                    }
                }
            }

            neighbors = neighborList.ToArray();
            return neighbors;
        }



        private static Coordinate4D[] neighbors;
    }
    #endregion Coordinates

    #region Point2D
    public record Point2D<T>(T X, T Y) : IComparable<Point2D<T>> where T : INumber<T>, INumberBase<T>
    {
        private static readonly T TwoT = T.One + T.One;
        public static readonly Point2D<T> Origin = (T.Zero, T.Zero);
        public static readonly Point2D<T> Up = (T.Zero, -T.One);
        public static readonly Point2D<T> Down = (T.Zero, T.One);
        public static readonly Point2D<T> Left = (-T.One, T.Zero);
        public static readonly Point2D<T> Right = (T.One, T.Zero);

        public override string ToString() => $"{X},{Y}";

        public static implicit operator (T, T)(Point2D<T> pt) => (pt.X, pt.Y);
        public static implicit operator Point2D<T>((T, T) t) => new(t.Item1, t.Item2);

        public static Point2D<T> operator -(Point2D<T> a)
            => new(-a.X, -a.Y);
        public static Point2D<T> operator -(Point2D<T> a, Point2D<T> b)
            => new(a.X - b.X, a.Y - b.Y);
        public static Point2D<T> operator +(Point2D<T> a, Point2D<T> b)
            => new(a.X + b.X, a.Y + b.Y);
        public static Point2D<T> operator *(Point2D<T> a, T b)
            => new(a.X * b, a.Y * b);
        public static Point2D<T> operator *(T a, Point2D<T> b)
            => b * a;
        public static Point2D<T> operator /(Point2D<T> a, T b)
            => new(a.X / b, a.Y / b);
        public static Point2D<T> operator %(Point2D<T> a, T b)
            => new(a.X % b, a.Y % b);
        public static Point2D<T> operator %(Point2D<T> a, Point2D<T> b)
            => new(a.X % b.X, a.Y % b.Y);

        public IEnumerable<Point2D<T>> Neighbours(bool withDiagonal = false)
        {
            yield return this + Right;
            yield return this + Down;
            yield return this + Up;
            yield return this + Left;
            if (withDiagonal)
            {
                yield return this + Up + Left;
                yield return this + Up + Right;
                yield return this + Down + Left;
                yield return this + Down + Right;
            }
        }

        public T ManhattanDistance(Point2D<T> other)
            => T.Abs(other.X - X) + T.Abs(other.Y - Y);

        public static IEnumerable<Point2D<T>> Range(Point2D<T> min, Point2D<T> max)
        {
            (var minX, var maxX) = min.X < max.X ? (min.X, max.X) : (max.X, min.X);
            (var minY, var maxY) = min.Y < max.Y ? (min.Y, max.Y) : (max.Y, min.Y);
            for (T x = minX; x <= maxX; x++)
                for (T y = minY; y <= maxY; y++)
                    yield return new(x, y);
        }

        public int CompareTo(Point2D<T> other)
        {
            var dy = Y - other.Y;
            if (dy == T.Zero)
                return T.Sign(X - other.X);
            return T.Sign(dy);
        }

        public static Point2D<T> Parse(string s)
        {
            var parts = s.Split(',');
            var fp = CultureInfo.InvariantCulture;
            return new Point2D<T>(T.Parse(parts[0], fp), T.Parse(parts[1], fp));
        }

        /// <summary>
        /// Maps '<', '>', '^' and 'v' into directions. Any other char returns (0,0)
        /// </summary>
        /// <param name="c"></param>
        /// <returns></returns>
        public static Point2D<T> DirectionFromArrow(char c)
            => c switch
            {
                '^' => Up,
                'v' => Down,
                '<' => Left,
                '>' => Right,
                _ => Origin
            };
    }
    #endregion Point2D

    #region StringMap
    public class StringMap<T> : IEnumerable<(Point2D<int> Index, T Value)>
    {
        private readonly T[,] fValues;

        public int Width => fValues.GetLength(0);
        public int Height => fValues.GetLength(1);
        public Point2D<int> Size => (Width, Height);

        public T this[Point2D<int> idx]
        {
            get => fValues[idx.X, idx.Y];
            set => fValues[idx.X, idx.Y] = value;
        }

        public bool Contains(Point2D<int> idx)
            => idx.X >= 0 && idx.Y >= 0 && idx.X < Width && idx.Y < Height;

        public StringMap(string input, Func<char, T> selector)
        {
            var lines = input.Lines().ToArray();
            int lineLen = lines[0].Length;
            fValues = new T[lineLen, lines.Length];
            for (int y = 0; y < lines.Length; y++)
            {
                var line = lines[y];
                for (int x = 0; x < lineLen; x++)
                    fValues[x, y] = selector(line[x]);
            }
        }

        public T GetValueOrDefault(Point2D<int> idx)
            => GetValueOrDefault(idx, default);
        public T GetValueOrDefault(Point2D<int> idx, T defaultValue)
        {
            if (Contains(idx))
                return this[idx];
            return defaultValue;
        }

        public bool TryGetValue(Point2D<int> idx, out T value)
        {
            if (Contains(idx))
            {
                value = this[idx];
                return true;
            }
            value = default;
            return false;
        }

        public IEnumerable<IEnumerable<T>> Rows()
            => Enumerable.Range(0, Height)
                .Select(y => Enumerable.Range(0, Width).Select(x => fValues[x, y]));


        private IEnumerable<(Point2D<int> Index, T Value)> GetIndexedValues()
        {
            for (int x = 0; x < Width; x++)
                for (int y = 0; y < Height; y++)
                    yield return ((x, y), fValues[x, y]);
        }

        public IEnumerator<(Point2D<int> Index, T Value)> GetEnumerator() => GetIndexedValues().GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => GetIndexedValues().GetEnumerator();
    }
    #endregion StringMap

    #region Ranges
    public class Range
    {
        public long Start;
        public long End;
        public long Len => End - Start + 1;

        public Range(long Start, long End)
        {
            this.Start = Start;
            this.End = End;
        }

        //Forced Deep Copy
        public Range(Range other)
        {
            this.Start = other.Start;
            this.End = other.End;
        }

        public override string ToString()
        {
            return $"[{Start}, {End}] ({Len})";
        }
    }

    public class MultiRange
    {
        public List<Range> Ranges = new();

        public MultiRange() { }

        public MultiRange(IEnumerable<Range> Ranges)
        {
            this.Ranges = new(Ranges);
        }

        public MultiRange(MultiRange other)
        {
            foreach (var r in other.Ranges)
            {
                Range n = new(r);
                Ranges.Add(n);
            }
        }

        public long len => Ranges.Aggregate(1L, (a, b) => a *= b.Len);
    }

    public class DictMultiRange<T>
    {
        public Dictionary<T, Range> Ranges = new();

        public DictMultiRange() { }

        public DictMultiRange(DictMultiRange<T> other)
        {
            foreach (var r in other.Ranges)
            {
                Range n = new(r.Value);
                Ranges[r.Key] = n;
            }
        }

        public long len => Ranges.Aggregate(1L, (a, b) => a *= b.Value.Len);
    }
    #endregion Ranges
}
