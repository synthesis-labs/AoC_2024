namespace AdventOfCode;

public class Day17 : BaseDay
{
    private readonly string[] _input;
    private int regA = 0;
    private int regB = 0;
    private int regC = 0;
    private long a = 0;
    private long b = 0;
    private long c = 0;
    private readonly List<int> Operands = new List<int>();
    private long res = 0;
    private string testString = "";
    public Day17()
    {
        _input = File.ReadAllLines(InputFilePath);
        foreach (var line in _input)
        {
            if (line.Contains("Register A: "))
            {
                var splits = line.Split("Register A: ");
                regA = int.Parse(splits[1]);
            }
            else if (line.Contains("Register B: "))
            {
                var splits = line.Split("Register B: ");
                regB = int.Parse(splits[1]);
            }
            else if (line.Contains("Register C: "))
            {
                var splits = line.Split("Register C: ");
                regC = int.Parse(splits[1]);
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                var splits = line.Split("Program: ");
                testString = splits[1];
                var nums = splits[1].Split(',');
                Operands = new List<int>(nums.Select(q => int.Parse(q)));
            }
        }
    }

    private List<int> RunProgram(int regA, int regB, int regC, List<int> ops)
    {
        var RegisterA = regA;
        var RegisterB = regB;
        var RegisterC = regC;

        var outList = new List<int>();
        for (var i = 0; i < ops.Count - 1; i += 2)
        {
            switch (ops[i])
            {
                case 0:
                    if (ops[i+1] < 4)
                    {
                        RegisterA = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, ops[i+1])));
                    }
                    else if (ops[i+1] == 4)
                    {
                        RegisterA = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterA)));
                    }
                    else if (ops[i+1] == 5)
                    {
                        RegisterA = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterB)));
                    }
                    else if (ops[i+1] == 6)
                    {
                        RegisterA = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterC)));
                    }
                    break;
                case 1:
                    RegisterB = RegisterB ^ ops[i+1];
                    break;
                case 2:
                    if (ops[i+1] < 4)
                    {
                        RegisterB = ops[i+1] % 8;
                    }
                    else if (ops[i+1] == 4)
                    {
                        RegisterB = RegisterA % 8;
                    }
                    else if (ops[i+1] == 5)
                    {
                        RegisterB = RegisterB % 8;
                    }
                    else if (ops[i+1] == 6)
                    {
                        RegisterB = RegisterC % 8;
                    }
                    break;
                case 3:
                    if (RegisterA != 0)
                    {
                        i = ops[i+1]-2;
                    }
                    break;
                case 4:
                    RegisterB = RegisterB ^ RegisterC;
                    break;
                case 5:
                    if (ops[i+1] < 4)
                    {
                        outList.Add(ops[i+1] % 8);
                    }
                    else if (ops[i+1] == 4)
                    {
                        outList.Add(RegisterA % 8);
                    }
                    else if (ops[i+1] == 5)
                    {
                        outList.Add(RegisterB % 8);
                    }
                    else if (ops[i+1] == 6)
                    {
                        outList.Add(RegisterC % 8);
                    }
                    break;
                case 6:
                    if (ops[i+1] < 4)
                    {
                        RegisterB = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, ops[i+1])));
                    }
                    else if (ops[i+1] == 4)
                    {
                        RegisterB = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterA)));
                    }
                    else if (ops[i+1] == 5)
                    {
                        RegisterB = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterB)));
                    }
                    else if (ops[i+1] == 6)
                    {
                        RegisterB = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterC)));
                    }
                    break;
                case 7:
                    if (ops[i+1] < 4)
                    {
                        RegisterC = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, ops[i+1])));
                    }
                    else if (ops[i+1] == 4)
                    {
                        RegisterC = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterA)));
                    }
                    else if (ops[i+1] == 5)
                    {
                        RegisterC = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterB)));
                    }
                    else if (ops[i+1] == 6)
                    {
                        RegisterC = Convert.ToInt32(Math.Floor(RegisterA / Math.Pow(2, RegisterC)));
                    }
                    break;
            }
        }
        return outList;
    }

    private string ProcessInput1()
    {
        var outList = RunProgram(regA, regB, regC, Operands);
        return $"{string.Join(",", outList)}";
    }

    private bool Solve(int len, long test)
    {
        if (len < 0)
        {
            res = test;
            return true;
        }

        long target = 0;
        for (int d = 0; d < 8; d++)
        {
            a = test * 8 | d;
            var i = 0;
            while(i < Operands.Count)
            {
                var hit5 = false;
                switch (Operands[i])
                {
                    case 0:
                        if (Operands[i + 1] <= 3)
                            a = Convert.ToInt64(Math.Floor(a / Math.Pow(2, Operands[i+1])));
                        else if (Operands[i + 1] == 4) a = Convert.ToInt64(Math.Floor(a / Math.Pow(2, a)));
                        else if (Operands[i + 1] == 5) a = Convert.ToInt64(Math.Floor(a / Math.Pow(2, b)));
                        else if (Operands[i + 1] == 6) a = Convert.ToInt64(Math.Floor(a / Math.Pow(2, c)));
                        break;
                    case 1:
                        b ^= Operands[i + 1];
                        break;
                    case 2:
                        if (Operands[i + 1] <= 3)
                            b = Operands[i + 1] % 8;
                        else if (Operands[i + 1] == 4) b = a % 8;
                        else if (Operands[i + 1] == 5) b = b % 8;
                        else if (Operands[i + 1] == 6) b = c % 8;
                        break;
                    case 3:
                        i = (a != 0) ? Operands[i + 1] - 2 : i;
                        break;
                    case 4:
                        b ^= c;
                        break;
                    case 5:
                        if (Operands[i + 1] <= 3)
                            target = Operands[i + 1] % 8;
                        else if (Operands[i + 1] == 4) target = a % 8;
                        else if (Operands[i + 1] == 5) target = b % 8;
                        else if (Operands[i + 1] == 6) target = c % 8;
                        hit5 = true;
                        break;
                    case 6:
                        if (Operands[i + 1] <= 3)
                            b = Convert.ToInt64(Math.Floor(a / Math.Pow(2, Operands[i + 1])));
                        else if (Operands[i + 1] == 4) b = Convert.ToInt64(Math.Floor(a / Math.Pow(2, a)));
                        else if (Operands[i + 1] == 5) b = Convert.ToInt64(Math.Floor(a / Math.Pow(2, b)));
                        else if (Operands[i + 1] == 6) b = Convert.ToInt64(Math.Floor(a / Math.Pow(2, c)));
                        break;
                    case 7:
                        if (Operands[i + 1] <= 3)
                            c = Convert.ToInt64(Math.Floor(a / Math.Pow(2, Operands[i + 1])));
                        else if (Operands[i + 1] == 4) c = Convert.ToInt64(Math.Floor(a / Math.Pow(2, a)));
                        else if (Operands[i + 1] == 5) c = Convert.ToInt64(Math.Floor(a / Math.Pow(2, b)));
                        else if (Operands[i + 1] == 6) c = Convert.ToInt64(Math.Floor(a / Math.Pow(2, c)));
                        break;
                };
                if (hit5) break;
                i += 2;
            }

            if (target == Operands[len] && Solve(len - 1, test * 8 | d))
            {
                return true;
            }
        }
        return false;
    }

    private string ProcessInput2()
    {
        Solve(Operands.Count -1, 0);
        return $"{res}";
    }
    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());

    public class Path
    {
        public char path { get; set; }
        public int visited { get; set; }
    }
}
