Console.WriteLine("Running AOC 2024 Day 02");

List<List<int>> GetInput()
{
    try
    {
        List<List<int>> totalInput = new List<List<int>>();
        StreamReader reader = new StreamReader("../../../input.txt");

        //read first line so no breaky
        var line = reader.ReadLine();

        while (line != null)
        {
            var lineValue = line.Split(" ").ToList().Select(x => int.Parse(x)).ToList();
            totalInput.Add(lineValue);
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

bool IsAssending(List<int> report)
{
    for (int i = 0; i < report.Count; i++) {
        if (i+1 != report.Count && report[i] != report[i+1] && report[i] < report[i + 1])
        {
            return true;
        }
    }
    return false;
}

bool IsReportFollowingPattern(List<int> report)
{
    var isAssending = IsAssending(report);
    if (isAssending)
    {
        for (int i = 0; i < report.Count; i++)
        {
            if (i + 1 != report.Count && report[i] > report[i + 1])
            {
                return false;
            }
        }
        return true;
    }
    else
    {
        for (int i = 0; i < report.Count; i++)
        {
            if (i + 1 != report.Count && report[i] < report[i + 1])
            {
                return false;
            }
        }
        return true;
    }
}

bool IsReportInRange(List<int> report)
{
    for (int i = 0; i < report.Count; i++)
    {
        if(i + 1 != report.Count && report[i] == report[i + 1])
        {
            return false;
        }

        if(i + 1 != report.Count)
        {
            var diff = report[i] - report[i + 1];
            diff = diff > 0 ? diff : diff * -1;
            if (diff > 3 || diff < 1)
            {
                return false;
            }
        }
    }
    return true;
}

string GetPart1()
{
    var safeReports = 0;
    var data = GetInput();

    foreach (var report in data)
    {
        if(IsReportFollowingPattern(report) && IsReportInRange(report))
        {
            safeReports++;
        }
    }

    return safeReports.ToString();
}

string GetPart2()
{
    var answer = 0;


    return answer.ToString();
}

Console.WriteLine("Part1: " + GetPart1());
Console.WriteLine("Part2: " + GetPart2());
Console.ReadLine();