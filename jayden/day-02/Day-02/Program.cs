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
    var numIncrease = 0;
    var numDecrease = 0;
    for (int i = 0; i < report.Count-1; i++)
    {
        if (report[i] < report[i + 1])
        {
            numIncrease++;
        }
        else
        {
            numDecrease++;
        }
    }
    return numIncrease > numDecrease;
}

bool IsReportFollowingPattern(List<int> report, bool? isAssendingPassin = null)
{
    var isAssending = false;
    if (isAssendingPassin == null)
    {
        isAssending = IsAssending(report);
    }
    else
    {
        isAssending = (bool)isAssendingPassin;
    }
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
        if (IsReportFollowingPattern(report) && IsReportInRange(report))
        {
            safeReports++;
        }
    }

    return safeReports.ToString();
}

string GetPart2()
{
    var safeReports = 0;
    var data = GetInput();
    foreach (var report in data)
    {
        var reportCopy = report.Select(x => x).ToList();
        var isValid = false;
        for (int i = 0; i < reportCopy.Count && !isValid; i++)
        {
            isValid = IsReportFollowingPattern(reportCopy.Where((item, idx) => idx != i).ToList())
                && IsReportInRange(reportCopy.Where((item, idx) => idx != i).ToList());
            if (isValid)
            {
                safeReports++;
                break;
            }
        }
    }
    return safeReports.ToString();
}

Console.WriteLine("Part1: " + GetPart1());
Console.WriteLine("Part2: " + GetPart2());