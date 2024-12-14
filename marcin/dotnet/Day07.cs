using MathEvaluation.Extensions;
using System.Text.RegularExpressions;

internal class Day07: IDay
{
    private readonly List<Equation> _equations;
    public Day07(List<string> data) 
    {
        _equations = data.Select(d =>
        {
            var split = d.Split(':');
            var (val, terms) = (double.Parse(split.First()), split[1].Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(s => double.Parse(s.Trim())));
            return new Equation { Value = val, Terms = terms?.ToList() };
        }).ToList();
    }

    class Equation
    {
        public double Value { get; set; }
        public List<double> Terms { get; set; }
        public bool Valid { get; set; }
        public List<string> ValidEquations { get; set; } = new List<string>();
    }

    public double Part1(List<string> data)
    {
        double sum = 0;
        foreach (var item in _equations)
        {
            var eq = new HashSet<string>();
            foreach (var term in item.Terms)
            {
                if (eq.Count == 0) eq.Add($"{term}");
                else
                {
                    var newEq = new HashSet<string>();
                    foreach (var q in eq)
                    {
                        newEq.Add($"({q}+{term})");
                        newEq.Add($"({q}*{term})");
                    }
                    foreach (var neq in newEq)
                    {
                        eq.Add(neq);
                    }
                }
            }

            var isValid = eq.Where(q => Regex.Count(q, "\\d+") == item.Terms.Count()).Aggregate(false, (valid, q) =>
            {
                try
                {
                    double answer = q.Evaluate();
                    if (answer == item.Value) item.ValidEquations.Add(q);
                    return valid || answer == item.Value;
                }
                catch (Exception ex)
                {
                    throw ex;
                }
                
            });

            sum += isValid ? item.Value : 0;
            item.Valid = isValid;
        }

        return sum;
    }

    public double Part2(List<string> data)
    {
        double sum = 0;
        int index = 1;
        foreach (var item in _equations)
        {
            var eq = new HashSet<string>();
            foreach (var term in item.Terms)
            {
                try { 
                    if (eq.Count == 0) eq.Add($"({term})");
                    else
                    {
                        var newEq = new HashSet<string>();
                        foreach (var q in eq)
                        {
                            newEq.Add($"({q}+{term})");
                            newEq.Add($"({q}*{term})");
                            newEq.Add($"({q}||{term})");
                        }
                        foreach (var neq in newEq)
                        {
                            eq.Add(neq);
                        }
                    }
                } catch (Exception ex)
                {
                    throw ex;
                }
            }

            var validExpressions = eq.Where(q => Regex.Count(q, "\\d+") == item.Terms.Count()).ToList();
            var isValid = eq.Where(q => Regex.Count(q, "\\d+") == item.Terms.Count()).Aggregate(false, (valid, q) =>
            {
                try
                {
                    string expression = "";
                    var matches = Regex.Matches(q, "(\\d+|\\+|\\*|\\|)");
                    foreach (var match in matches)
                    {

                        if (Regex.Match(match.ToString(), "(\\+|\\*)").Success) 
                        {
                            expression += match.ToString();
                        }
                        if (Regex.Match(match.ToString(), "\\d+").Success)
                        {
                            expression += match.ToString();
                            expression = expression.Evaluate().ToString();
                        }
                        if (Regex.Match(match.ToString(), "\\|").Success)
                        {
                            expression = expression.Evaluate().ToString();
                        }
                    }
                    var answer = double.Parse(expression);
                    if (answer == item.Value) item.ValidEquations.Add(q);
                    return valid || answer == item.Value;
                }
                catch (Exception ex)
                {
                    throw ex;
                }

            });

            sum += isValid ? item.Value : 0;
            item.Valid = isValid;
        }

        return sum;
    }
}