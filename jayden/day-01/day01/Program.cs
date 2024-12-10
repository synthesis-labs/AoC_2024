Console.WriteLine("Running AOC 2024 Day 01-1");

List<string> GetInput()
{
    try
    {
        List<string> totalInput = new List<string>();
        StreamReader reader = new StreamReader("../../../input1.txt");

        //read first line so no breaky
        var line = reader.ReadLine();

        while (line != null)
        {
            totalInput.Add(line);
            line = reader.ReadLine();
        }

        //close connection so no breaky
        reader.Close();
        return totalInput;
    }
    catch (Exception e)
    {
        Console.WriteLine(e);
        throw;
    }
}

string GetPart1()
{
    try
    {
        var input = GetInput();
        var distanceTotal = 0;
        var leftList = new List<int>();
        var rightList = new List<int>();

        foreach (var line in input)
        {
            var inputValue = line.Split("   ");
            leftList.Add(int.Parse(inputValue[0]));
            rightList.Add(int.Parse(inputValue[1]));
        }

        leftList.Sort();
        rightList.Sort();
        
        for (int i = 0; i < input.Count; i++)
        {
            var diff = 0;
            diff = leftList[i] - rightList[i];
            diff = diff < 0 ? diff * -1 : diff;
            distanceTotal =+ distanceTotal + diff;
        }
        return distanceTotal.ToString(); ;
    }
    catch (Exception e)
    {
        Console.WriteLine(e);
        throw;
    }
}

string GetPart2()
{
    var input = GetInput();
    var similarityScore = 0;
    var leftList = new List<int>();
    var rightList = new List<int>();

    foreach (var line in input)
    {
        var inputValue = line.Split("   ");
        leftList.Add(int.Parse(inputValue[0]));
        rightList.Add(int.Parse(inputValue[1]));
    }

    leftList.Sort();
    rightList.Sort();

    foreach (var leftValue in leftList)
    {
        var caclValue = rightList
            .Where(x => x == leftValue)
            .ToList()
            .Count() * leftValue;
        similarityScore =+ similarityScore + caclValue;
    }

    return similarityScore.ToString();
}

Console.WriteLine("Part1: " + GetPart1());
Console.WriteLine("Part2: " + GetPart2());
Console.ReadLine();