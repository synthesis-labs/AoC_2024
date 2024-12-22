﻿namespace AdventOfCode;

public class Day21 : BaseDay
{
    private readonly string[] _input;
    private Dictionary<long, long> seenMoves = new Dictionary<long, long>();
    private Queue<(int x, int y, string moves)> playerMoves = new Queue<(int x, int y, string moves)>();
    private char[][] Keypad;
    private char[][] BotPad;
    public Day21()
    {
        _input = File.ReadAllLines(InputFilePath);
        Keypad = new char[4][];
        Keypad[0] = new char[3];
        Keypad[1] = new char[3];
        Keypad[2] = new char[3];
        Keypad[3] = new char[3];
        Keypad[0][0] = '7';
        Keypad[0][1] = '8';
        Keypad[0][2] = '9';
        Keypad[1][0] = '4';
        Keypad[1][1] = '5';
        Keypad[1][2] = '6';
        Keypad[2][0] = '1';
        Keypad[2][1] = '2';
        Keypad[2][2] = '3';
        Keypad[3][0] = '\0';
        Keypad[3][1] = '0';
        Keypad[3][2] = 'A';
        BotPad = new char[2][];
        BotPad[0] = new char[3];
        BotPad[1] = new char[3];
        BotPad[0][0] = '\0';
        BotPad[0][1] = '^';
        BotPad[0][2] = 'A';
        BotPad[1][0] = '<';
        BotPad[1][1] = 'v';
        BotPad[1][2] = '>';

    }

    private long LowestCostPad(int curx, int cury, int targetx, int targety, int numPads)
    {
        long seen = (((((curx * 3) + cury) * 3 + targetx) * 3 + targety) * 25) + numPads;

        if (seenMoves.ContainsKey(seen)) return seenMoves[seen];

        long res = long.MaxValue;
        var robotMoves = new Queue<(int x, int y, string moves)>();
        robotMoves.Enqueue((curx, cury, ""));

        while (robotMoves.TryDequeue(out var test))
        {
            if (test.x == targetx && test.y == targety)
            {
                long recorded = LowestCostRobot(test.moves + "A", numPads - 1);
                res = Math.Min(res, recorded);
                continue;
            }

            if (test.x == 0 && test.y == 0)
                continue;

            if (test.x < targetx)
                robotMoves.Enqueue((test.x + 1, test.y, test.moves + 'v'));
            if (test.x > targetx)
                robotMoves.Enqueue((test.x - 1, test.y, test.moves + '^'));
            if (test.y < targety)
                robotMoves.Enqueue((test.x, test.y + 1, test.moves + '>'));
            if (test.y > targety)
                robotMoves.Enqueue((test.x, test.y - 1, test.moves + '<'));
        }

        seenMoves[seen] = res;
        return res;
    }

    private long LowestCostRobot(string moves, int numPads)
    {
        if (numPads == 1)
            return moves.Length;

        long res = 0;
        int startx = 0;
        int starty = 2;

        for (int j = 0; j < moves.Length; j++)
        {
            var seen = false;
            for (int nextx = 0; nextx < 2; nextx++)
            {
                for (int nexty = 0; nexty < 3; nexty++)
                {
                    if (BotPad[nextx][nexty] == moves[j])
                    {
                        res += LowestCostPad(startx, starty, nextx, nexty, numPads);
                        startx = nextx;
                        starty = nexty;
                        seen = true;
                        break;
                    }
                }
                if (seen) break;
            }
        }
        return res;
    }

    private long LowestCost(int curx, int cury, int targetx, int targety, int numPads)
    {
        long res = long.MaxValue;
        playerMoves.Clear();
        playerMoves.Enqueue((curx, cury, ""));

        while (playerMoves.TryDequeue(out var test))
        {
            if (test.x == targetx && test.y == targety)
            {
                long recorded = LowestCostRobot(test.moves + "A", numPads);
                res = Math.Min(res, recorded);
                continue;
            }

            if (test.x == 3 && test.y == 0) continue;

            if (test.x < targetx)
                playerMoves.Enqueue((test.x + 1, test.y, test.moves + 'v'));
            if (test.x > targetx)
                playerMoves.Enqueue((test.x - 1, test.y, test.moves + '^'));
            if (test.y < targety)
                playerMoves.Enqueue((test.x, test.y + 1, test.moves + '>'));
            if (test.y > targety)
                playerMoves.Enqueue((test.x, test.y - 1, test.moves + '<'));
        }
        return res;
    }

    private string ProcessKeypad(int numPads)
    {
        long sum = 0;
        foreach (var line in _input)
        {
            long res = 0;
            int startx = 3;
            int starty = 2;

            foreach (char c in line)
            {
                var seen = false;
                for (int nextx = 0; nextx < 4; nextx++)
                {
                    for (int nexty = 0; nexty < 3; nexty++)
                    {
                        if (Keypad[nextx][nexty] == c)
                        {
                            res += LowestCost(startx, starty, nextx, nexty, numPads);
                            startx = nextx;
                            starty = nexty;
                            seen = true;
                            break;
                        }
                    }
                    if (seen) break;
                }
            }

            sum += res * int.Parse(line.Substring(0, 3));
        }

        return $"{sum}";
    }

    private string ProcessBotPad(int numPads)
    {
        long sum = 0;
        foreach (var line in _input)
        {
            long res = 0;
            int startx = 3;
            int starty = 2;

            foreach (char c in line)
            {
                var seen = false;
                for (int nextx = 0; nextx < 4; nextx++)
                {
                    for (int nexty = 0; nexty < 3; nexty++)
                    {
                        if (Keypad[nextx][nexty] == c)
                        {
                            res += LowestCost(startx, starty, nextx, nexty, numPads);
                            startx = nextx;
                            starty = nexty;
                            seen = true;
                            break;
                        }
                    }
                    if (seen) break;
                }
            }

            sum += res * int.Parse(line.Substring(0, 3));
        }

        return $"{sum}";
    }


    public override ValueTask<string> Solve_1() => new(ProcessKeypad(3));

    public override ValueTask<string> Solve_2() => new(ProcessBotPad(26));


}
