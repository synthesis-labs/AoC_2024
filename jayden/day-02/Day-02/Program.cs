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

int FindErrorPosInPattern(List<int> report)
{
    var reportCopy = report;
    var isAssending = IsAssending(reportCopy);
    if (isAssending)
    {
        for (int i = 1; i < report.Count-1; i++)
        {
            if (i + 1 != report.Count && report[i] > report[i + 1] || report[i] < report[i - 1])
            {
                var currentPosVal = report[i];
                var nextPosVal = report[i + 1];
                var prevPosVal = report[i - 1];

                if(currentPosVal > nextPosVal)
                {
                    return i + 1;
                }
                if(currentPosVal < prevPosVal)
                {
                    return i - 1;
                }
                return i;
            }
        }
    }
    else
    {
        for (int i = 1; i < report.Count-1; i++)
        {
            if (i + 1 != report.Count && (report[i] < report[i + 1] || report[i] > report[i-1] ))
            {
                var currentPosVal = report[i];
                var nextPosVal = report[i + 1];
                var prevPosVal = report[i - 1];

                if (currentPosVal < nextPosVal)
                {
                    return i + 1;
                }
                if (currentPosVal > prevPosVal)
                {
                    return i - 1;
                }
                return i;
            }
        }
    }
    return -1;
}

bool IsReportFollowingPatternWithOneError(List<int> report)
{
    var errorPos = FindErrorPosInPattern(report);

    if(errorPos != -1)
    {
        //rerun to check pattern excluding the already found error
        report.RemoveAt(errorPos);
        return IsReportFollowingPattern(report);
    }
    else
    {
        return true;
    }
}

int FindErrorPosInRange(List<int> report)
{
    for (int i = 1; i < report.Count - 1; i++)
    {
        if(i == 6)
        {

        }
        var prevPost = report[i - 1];
        var currentPos = report[i];
        var nextPos = report[i + 1];

        var isGapDiffSafe = false;
        var isPrevDiffSafe = false;
        var isNextDiffSafe = false;

        var prevDiff = report[i] - report[i - 1];
        prevDiff = prevDiff > 0 ? prevDiff : prevDiff * -1;

        var gapDiff = report[i - 1] - report[i + 1];
        gapDiff = gapDiff > 0 ? gapDiff : gapDiff * -1;

        var nextDiff = report[i] - report[i + 1];
        nextDiff = nextDiff > 0 ? nextDiff : nextDiff * -1;

        if (gapDiff <= 3 && gapDiff >= 1)
        {
            isGapDiffSafe = true;
        }
        if (prevDiff <= 3 && prevDiff >= 1)
        {
            isPrevDiffSafe = true;
        }
        if (nextDiff <= 3 && nextDiff >= 1)
        {
            isNextDiffSafe = true;
        }

        if(isPrevDiffSafe && isNextDiffSafe)
        {
            //idk bro
        }
        else
        {
            if (!isPrevDiffSafe && isNextDiffSafe)
            {
                return i - 1;
            }
            if(isPrevDiffSafe && !isNextDiffSafe)
            {
                return i + 1;
            }
            if (isGapDiffSafe)
            {
                return i;
            }

            if (isGapDiffSafe)
            {
                return i;
            }

            //if (!isNextDiffSafe)
            //{
            //    if (i + 2 < report.Count())
            //    {
            //        //if the gap between next next is safe, next is problem
            //        var nextNextDiff = report[i] - report[i + 2];
            //        nextNextDiff = nextNextDiff > 0 ? nextNextDiff : nextNextDiff * -1;
            //        if (nextNextDiff <= 3 && nextNextDiff >= 1)
            //        {
            //            return i + 1;
            //        }
            //    }
            //    else
            //    {
            //        //if there is no further numbers, the next one is the issue
            //        return i + 1;
            //    }
            //}

            //if (!isPrevDiffSafe)
            //{
            //    if (i - 2 >= 0)
            //    {
            //        var prevPrevDiff = report[i] - report[i - 2];
            //        prevPrevDiff = prevPrevDiff > 0 ? prevPrevDiff : prevPrevDiff * -1;
            //        if (prevPrevDiff <= 3 && prevPrevDiff >= 1)
            //        {
            //            return i + 1;
            //        }
            //    }
            //    else
            //    {
            //        //if there is nothing 2 before i thats the issue, i-1 is issue
            //        return i - 1;
            //    }
            //}
    }
        //    if (i + 1 != report.Count && report[i] == report[i + 1])
        //    {
        //        //find error pos for equal nums
        //        return i;//if the nums are the same, it does matter which pos
        //    }

        //    if (i + 1 != report.Count)
        //    {
        //        //find if diff is 3
        //        var diff = report[i] - report[i + 1];
        //        diff = diff > 0 ? diff : diff * -1;
        //        if (diff > 3 || diff < 1)
        //        {
        //            //error pos is i
        //            if (i + 1 == report.Count)
        //            {
        //                return i;
        //            }
        //            if(i + 1 == report.Count - 1)
        //            {
        //                return i + 1;
        //            }
        //            if (i - 1 >= 0 && i + 1 < report.Count())
        //            {
        //                var newDiff = report[i - 1] - report[i + 1];
        //                diff = diff > 0 ? diff : diff * -1;
        //                if (diff > 3 || diff < 1)
        //                {
        //                    return i;
        //                }
        //            }

        //            //error pos is i+1
        //            if (i + 2 < report.Count())
        //            {
        //                var newDiff = report[i] - report[i + 2];
        //                diff = diff > 0 ? diff : diff * -1;
        //                if (diff > 3 || diff < 1)
        //                {
        //                    return i;
        //                }
        //            }
        //        }
        //    }
        //}
    }
    return -1;
}

bool IsReportInRangeWithOneError(List<int> report)
{
    var errorPos = FindErrorPosInRange(report);

    if(errorPos != -1)
    {
        report.RemoveAt(errorPos);
        return IsReportInRange(report);
    }
    else
    {
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
    var row = 0;
    foreach (var report in data)
    {
        row++;
        if (row == 2)
        {

        }
        var reportCopy = report;
        var numErrors = 0;
        var isAssending = IsAssending(reportCopy);

        var errorPos = FindErrorPosInPattern(reportCopy);
        if(errorPos != -1)
        {
            numErrors++;
            reportCopy.RemoveAt(errorPos);
            if (!IsReportFollowingPattern(reportCopy, isAssending))
            {
                numErrors++;
            }
            
        }

        if (numErrors > 1)
        {
            continue;
        }

        if (numErrors == 0) 
        {
            errorPos = FindErrorPosInRange(reportCopy);
            //error found in pattern
            if(errorPos != -1)
            {
                numErrors++;
                reportCopy.RemoveAt(errorPos);
                if (!IsReportInRange(reportCopy))
                {
                    numErrors++;
                }
            }
        }
        else
        {
            if (!IsReportInRange(reportCopy))
            {
                numErrors++;
            }
        }
        if(numErrors > 1)
        {
            continue;
        }
        Console.WriteLine(row);
        safeReports++;
    }

    return safeReports.ToString();
}

Console.WriteLine("Part1: " + GetPart1());
Console.WriteLine("Part2: " + GetPart2());