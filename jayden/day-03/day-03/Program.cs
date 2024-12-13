using System.Collections.Generic;
using System.Text.RegularExpressions;

List<string> GetInput()
{
    try
    {
        List<string> totalInput = new List<string>();
        StreamReader reader = new StreamReader("../../../input.txt");

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

string SolvePart1()
{
    var answer = 0;
    var input = GetInput();
    var validOperations = new List<string>();
    foreach (var line in input)
    {
        var filteredOutput = Regex.Matches(line, @"mul\(\d{1,3},\d{1,3}\)")
            .Select(x => Regex.Match(x.Value, @"mul\(").Value)
            .Select(y => Regex.Match(y, @"\)").Value)
            .ToList();
        //filteredOutput = filteredOutput.Select(x => x.).ToString());
        validOperations.AddRange(filteredOutput);
    }
    foreach (var operation in validOperations)
    {
        var num1 = 0;
        var num2 = 0;

        answer += num1 * num2;
    }

    return answer.ToString();
}

string SolvePart2()
{
    var answer = "";
    return answer;
}

Console.WriteLine("Part 1: " + SolvePart1());
Console.WriteLine("Part 2: " + SolvePart2());
