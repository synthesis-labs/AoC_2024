using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AOC2024
{
    internal class Day5
    {
        String[] lines = File.ReadAllLines("input.day5.txt");
        Dictionary<int, List<int>> pageOrderingRules = new Dictionary<int, List<int>>();
        List<List<int>> pageUpdates = new List<List<int>>();
        List<List<int>> pageUpdatesThatFollowTheRules = new List<List<int>>();
        List<List<int>> pageUpdatesThatDontFollowTheRules = new List<List<int>>();

        internal void ExecuteDay5Part1()
        {
            ParseInput();
            FindPageUpdatesThatFollowTheRules();
            int sumOfMiddlePages = AddTogetherMiddlePageNumbers(pageUpdatesThatFollowTheRules);
            Console.WriteLine("Day 5, Part 1: " + sumOfMiddlePages);
        }
        internal void ExecuteDay5Part2()
        {
            FixOrderingOfPagesThatDontFollowTheRules();
            int sumOfMiddlePages = AddTogetherMiddlePageNumbers(pageUpdatesThatDontFollowTheRules);
            Console.WriteLine("Day 5, Part 2: " + sumOfMiddlePages);
        }

        private void ParseInput()
        {
            bool processedPageOrderingRules = false;
            foreach (var line in lines)
            {
                if (line == "")
                {
                    processedPageOrderingRules = true;
                    continue;
                }

                if (processedPageOrderingRules)
                    pageUpdates.Add(line.Split(',').Select(x => Int32.Parse(x)).ToList());
                else
                {
                    var rule = line.Split('|').Select(x => Int32.Parse(x)).ToList();
                    if (pageOrderingRules.ContainsKey(rule[0]))
                        pageOrderingRules[rule[0]].Add(rule[1]);
                    else
                        pageOrderingRules.Add(rule[0], new List<int> { rule[1] });
                }

            }
        }

        private void FindPageUpdatesThatFollowTheRules()
        {
            pageUpdates
                .Select(x => (pageUpdates: x, followsTheRules: FollowsTheRules(x)))
                .ToList()
                .ForEach(x =>
                {
                    if (x.followsTheRules)
                        pageUpdatesThatFollowTheRules.Add(x.pageUpdates);
                    else
                        pageUpdatesThatDontFollowTheRules.Add(x.pageUpdates);
                });

        }

        private bool FollowsTheRules(List<int> pageUpdatesList)
        {
            bool followsTheRules = false;
            for (int i = 0; i < pageUpdatesList.Count; i++)
            {
                var pageUpdate = pageUpdatesList[i];
                if (pageOrderingRules.ContainsKey(pageUpdate))
                {
                    var rules = pageOrderingRules[pageUpdate];
                    if (pageUpdatesList.Take(i + 1).Intersect(rules).Count() == 0)
                        followsTheRules = true;
                    else
                    {
                        followsTheRules = false;
                        break;
                    }
                }
            }

            return followsTheRules;
        }

        private void FixOrderingOfPagesThatDontFollowTheRules()
        {
            for (int i = 0; i < pageUpdatesThatDontFollowTheRules.Count; i++)
            {
                var page = pageUpdatesThatDontFollowTheRules[i];
                for (int j = 0; j < page.Count; j++)
                {
                    var pageUpdate = page[j];
                    if (j == 0 || !pageOrderingRules.ContainsKey(pageUpdate)) continue;

                    var rules = pageOrderingRules[pageUpdate];
                    var previousPages = page.Take(j + 1).ToList();
                    var intersections = previousPages.Intersect(rules).ToList();

                    if (intersections.Count > 0)
                    {
                        page.RemoveAt(page.IndexOf(pageUpdate));
                        page.Insert(intersections.Min(x => page.IndexOf(x)), pageUpdate);
                    }
                }
            }
        }

        private int AddTogetherMiddlePageNumbers(List<List<int>> pageUpdates)
        {
            return pageUpdates.Sum(x => x.ElementAt((x.Count() - 1) / 2));
        }
    }
}
