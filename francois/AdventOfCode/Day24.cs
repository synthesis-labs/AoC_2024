namespace AdventOfCode;

public class Day24 : BaseDay
{
    private readonly string[] _input;
    private Dictionary<string, int> terminals = new Dictionary<string, int>();
    private HashSet<(string x, string y, string type, string output)> gates = new HashSet<(string x, string y, string type, string output)>();
    private HashSet<string> possibleSwaps = new HashSet<string>();
    private string originalz = string.Empty;
    private int zlength = 0;

    public Day24()
    {
        _input = File.ReadAllLines(InputFilePath);

        foreach(var line in _input)
        {
            if (line.Contains('>'))
            {
                var split = line.Split("->");
                var terms = split[0].Split(" ");
                gates.Add((terms[0], terms[2], terms[1], split[1].Trim()));
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                var split = line.Split(':');
                terminals.TryAdd(split[0], int.Parse(split[1].Trim()));
            }
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        var skippedGates = new HashSet<(string x, string y, string type, string output)>();
        foreach (var gate in gates)
        {
            if (!terminals.ContainsKey(gate.x) || !terminals.ContainsKey(gate.y))
                skippedGates.Add(gate);
            else
            {
                switch (gate.type)
                {
                    case "AND":
                        var checkand = terminals[gate.x] == terminals[gate.y] && terminals[gate.x] == 1 ? 1 : 0;
                        if (!terminals.TryAdd(gate.output, checkand))
                        {
                            terminals[gate.output] = checkand;
                        }
                        break;
                    case "OR":
                        var checkor = terminals[gate.x] == 1 || terminals[gate.y] == 1 ? 1 : 0;
                        if (!terminals.TryAdd(gate.output, checkor))
                        {
                            terminals[gate.output] = checkor;
                        }

                        break;
                    case "XOR":
                        var checkxor = (terminals[gate.x] == 1 && terminals[gate.y] == 0) || (terminals[gate.x] == 0 && terminals[gate.y] == 1) ? 1 : 0;
                        if (!terminals.TryAdd(gate.output, checkxor))
                        {
                            terminals[gate.output] = checkxor;
                        }

                        break;
                }
            }
        }

        var gateset = new HashSet<(string x, string y, string type, string output)>(skippedGates);
        skippedGates.Clear();
        while(gateset.Count > 0)
        {
            foreach (var gate in gateset)
            {
                if (!terminals.ContainsKey(gate.x) || !terminals.ContainsKey(gate.y))
                    skippedGates.Add(gate);
                else
                {
                    switch (gate.type)
                    {
                        case "AND":
                            var checkand = terminals[gate.x] == terminals[gate.y] && terminals[gate.x] == 1 ? 1 : 0;
                            if (!terminals.TryAdd(gate.output, checkand))
                            {
                                terminals[gate.output] = checkand;
                            }
                            break;
                        case "OR":
                            var checkor = terminals[gate.x] == 1 || terminals[gate.y] == 1 ? 1 : 0;
                            if (!terminals.TryAdd(gate.output, checkor))
                            {
                                terminals[gate.output] = checkor;
                            }

                            break;
                        case "XOR":
                            var checkxor = (terminals[gate.x] == 1 && terminals[gate.y] == 0) || (terminals[gate.x] == 0 && terminals[gate.y] == 1) ? 1 : 0;
                            if (!terminals.TryAdd(gate.output, checkxor))
                            {
                                terminals[gate.output] = checkxor;
                            }

                            break;
                    }
                }
            }
            gateset = new HashSet<(string x, string y, string type, string output)>(skippedGates);
            skippedGates.Clear();
        }

        originalz = string.Join("", terminals.Where(q => q.Key.StartsWith('z')).OrderByDescending(q => q.Key).Select(q => q.Value));
        sum = Convert.ToInt64(originalz, 2);
        return $"{sum}";
    }

    private string FindOutput(string a, string b, string operatorType)
    {
        return gates.FirstOrDefault(gate =>
            (gate.x == a && gate.y == b && gate.type == operatorType) || (gate.x == b && gate.y == a && gate.type == operatorType)).output;
    }

    private void CheckPossibleSwaps()
    {
        string firstcheck = string.Empty;
        for (int x = 0; x < zlength - 1; x++)
        {
            string num = $"{x:D2}";
            string firstgate = string.Empty;
            string secondgate = string.Empty;
            string andgate = string.Empty;
            string xorgate = string.Empty;
            string orgate = string.Empty;

            firstgate = FindOutput($"x{num}", $"y{num}", "XOR");
            secondgate = FindOutput($"x{num}", $"y{num}", "AND");

            if (!string.IsNullOrWhiteSpace(firstcheck))
            {
                andgate = FindOutput(firstcheck, firstgate, "AND");

                if (string.IsNullOrWhiteSpace(andgate))
                {
                    (secondgate, firstgate) = (firstgate, secondgate);
                    possibleSwaps.Add(firstgate);
                    possibleSwaps.Add(secondgate);
                    andgate = FindOutput(firstcheck, firstgate, "AND");
                }

                xorgate = FindOutput(firstcheck, firstgate, "XOR");

                if (firstgate.StartsWith('z'))
                {
                    (firstgate, xorgate) = (xorgate, firstgate);
                    possibleSwaps.Add(firstgate);
                    possibleSwaps.Add(xorgate);
                }

                if (secondgate.StartsWith('z'))
                {
                    (secondgate, xorgate) = (xorgate, secondgate);
                    possibleSwaps.Add(secondgate);
                    possibleSwaps.Add(xorgate);
                }

                if (andgate.StartsWith('z'))
                {
                    (andgate, xorgate) = (xorgate, andgate);
                    possibleSwaps.Add(andgate);
                    possibleSwaps.Add(xorgate);
                }

                orgate = FindOutput(andgate, secondgate, "OR");
            }

            if (orgate.StartsWith('z') == true && orgate != "z45")
            {
                (orgate, xorgate) = (xorgate, orgate);
                possibleSwaps.Add(orgate);
                possibleSwaps.Add(xorgate);
            }

            firstcheck = string.IsNullOrWhiteSpace(firstcheck) ? secondgate : orgate;
        }
    }

    private string ProcessInput2()
    {
        zlength = terminals.Keys.Where(q => q.StartsWith('z')).Count();
        CheckPossibleSwaps();
        return $"{string.Join(",", possibleSwaps.Order())}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());


}
