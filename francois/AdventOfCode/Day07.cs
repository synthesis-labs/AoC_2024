using Utilities;

namespace AdventOfCode;

public class Day07 : BaseDay
{
    private readonly HashSet<(long, List<long>)> testValues = new HashSet<(long, List<long>)>();
    private readonly string[] _input;
    private readonly List<char> operations = new List<char>() { '+', '*' };
    private readonly List<char> part2operations = new List<char>() { '+', '*', '|' };
    public Day07()
    {
        _input = File.ReadAllLines(InputFilePath);
        for (var x = 0; x < _input.Length; x++)
        {
            var split = _input[x].Split(':');
            testValues.Add((long.Parse(split[0]), split[1].ToLongList(" ")));
        }
    }
    private string ProcessInput1(string[] input)
    {
        long sum = 0;
        foreach (var value in testValues)
        {
            var isValid = false;

            var permlist = CreatePermutations(value.Item2.Count - 1, operations);

            isValid = permlist.Any(permutation => CheckValidity(value.Item1, value.Item2, permutation));

            // original MORE brute force approach for part 1 -- took way too long on actual dataset
            //var checkAllAdd = value.Item2.Sum();
            //if (checkAllAdd == value.Item1)
            //{
            //    isValid = true;
            //}
            //else if (value.Item2.Aggregate((x, y) => x * y) == value.Item1)
            //{
            //    isValid = true;
            //}
            //else
            //{
            //    var testVal = value.Item2[0];
            //    var testlist = new List<int>(value.Item2);
            //    testlist.RemoveAt(0);
            //    isValid = CheckPermutations(value.Item1, testlist, testVal);
            //}

            if (isValid)
            {
                sum += value.Item1;
            }
        }
        return $"{sum}";
    }
    private string ProcessInput2(string[] input)
    {
        long sum = 0;
        foreach (var value in testValues)
        //Parallel.ForEach(testValues, (value) =>
        {
            var isValid = false;

            var permlist = CreatePermutations(value.Item2.Count - 1, part2operations);

            isValid = permlist.Any(permutation => CheckValidity(value.Item1, value.Item2, permutation));

            if (isValid)
            {
                sum += value.Item1;
            }
        }
        return $"{sum}";
    }

    private bool CheckValidity(long target, List<long> numbers, List<char> permutations)
    {
        char perm = '+';
        long testval = 0;
        for (var i = 0; i < numbers.Count; i++)
        {
            testval = perm switch
            {
                '+' => testval += numbers[i],
                '*' => testval *= numbers[i],
                '|' => long.Parse($"{testval}{numbers[i]}")
            };

            if(i < permutations.Count)
            {
                perm = permutations[i];
            }
        }
        return testval == target;
    }

    private void PermutationHelper(List<List<char>> res, char[] cur, int index, List<char> operations)
    {
        if (index == cur.Length)
        {
            res.Add(new List<char>(cur));
        }
        else
        {
            foreach(var calc in operations)
            {
                cur[index] = calc;
                PermutationHelper(res, cur, index + 1, operations);
            }
        }

    }

    private List<List<char>> CreatePermutations(int len, List<char> operations)
    {
        var res = new List<List<char>>();
        PermutationHelper(res, new char[len], 0, operations);
        return res;
    }

    // original MORE brute force approach for part 1 -- took way too long on actual dataset
    //private bool CheckPermutations(long target, List<int> items, long curValue)
    //{
    //    foreach(var item in items)
    //    {
    //        var testList = new List<int>(items);
    //        testList.RemoveAt(0);
    //        var checkAdd = CheckPermutations(target, testList, curValue + items[0]);
    //        var checkMul = CheckPermutations(target, testList, curValue * items[0]);

    //        if(checkAdd || checkMul)
    //        {
    //            return true;
    //        }
    //    }

    //    if(target != curValue)
    //    {
    //        return false;
    //    }
    //    else
    //    {
    //        return true;
    //    }
    //}


    public override ValueTask<string> Solve_1() => new(ProcessInput1(_input));

    public override ValueTask<string> Solve_2() => new(ProcessInput2(_input));
}
