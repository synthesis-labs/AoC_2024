using Utilities;

namespace AdventOfCode;

public class Day15 : BaseDay
{
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map;
    (Dictionary<Coordinate2D, char> map, int maxX, int maxY) map2;
    List<List<Coordinate2D>> boxesToMove = new List<List<Coordinate2D>>();
    List<Coordinate2D> movedPositions = new List<Coordinate2D>();
    Queue<char> Moves = new Queue<char>();
    private readonly string movestring;
    private readonly string _input;
    public Day15()
    {
        _input = File.ReadAllText(InputFilePath);

        var split = _input.SplitByDoubleNewline();

        map = split[0].GenerateMap(false);

        split[0] = split[0].Replace("#", "##");
        split[0] = split[0].Replace("O", "[]");
        split[0] = split[0].Replace(".", "..");
        split[0] = split[0].Replace("@", "@.");

        map2 = split[0].GenerateMap(false);
        movestring = split[1];
    }

    private void SetupMoves()
    {
        foreach (var c in movestring)
        {
            if (c == '>' || c == '<' || c == '^' || c == 'v')
            {
                Moves.Enqueue(c);
            }
        }
    }

    private string ProcessInput1()
    {
        long sum = 0;
        SetupMoves();
        var cur = map.map.First(q => q.Value == '@').Key;
        while(Moves.TryDequeue(out char move))
        {
            switch (move)
            {
                case '>':
                    var right = cur.MoveDirection(CompassDirection.E);
                    if (map.map[right] == 'O')
                    {
                        var nextbox = right.MoveDirection(CompassDirection.E);
                        var canmove = TryMoveBox(CompassDirection.E, right, nextbox);
                        if (canmove)
                        {
                            map.map[cur] = '.';
                            map.map[right] = '@';
                            cur = right;
                        }
                    }
                    else if (map.map[right] == '.')
                    {
                        map.map[cur] = '.';
                        map.map[right] = '@';
                        cur = right;
                    }
                    break;
                case 'v':
                    var down = cur.MoveDirection(CompassDirection.N);
                    if (map.map[down] == 'O')
                    {
                        var nextbox = down.MoveDirection(CompassDirection.N);
                        var canmove = TryMoveBox(CompassDirection.N, down, nextbox);
                        if(canmove)
                        {
                            map.map[cur] = '.';
                            map.map[down] = '@';
                            cur = down;
                        }
                    }
                    else if (map.map[down] == '.')
                    {
                        map.map[cur] = '.';
                        map.map[down] = '@';
                        cur = down;
                    }
                    break;
                case '<':
                    var left = cur.MoveDirection(CompassDirection.W);
                    if (map.map[left] == 'O')
                    {
                        var nextbox = left.MoveDirection(CompassDirection.W);
                        var canmove = TryMoveBox(CompassDirection.W, left, nextbox);
                        if (canmove)
                        {
                            map.map[cur] = '.';
                            map.map[left] = '@';
                            cur = left;
                        }
                    }
                    else if (map.map[left] == '.')
                    {
                        map.map[cur] = '.';
                        map.map[left] = '@';
                        cur = left;
                    }
                    break;
                case '^':
                    var up = cur.MoveDirection(CompassDirection.S);
                    if (map.map[up] == 'O')
                    {
                        var nextbox = up.MoveDirection(CompassDirection.S);
                        var canmove = TryMoveBox(CompassDirection.S, up, nextbox);
                        if (canmove)
                        {
                            map.map[cur] = '.';
                            map.map[up] = '@';
                            cur = up;
                        }
                    }
                    else if (map.map[up] == '.')
                    {
                        map.map[cur] = '.';
                        map.map[up] = '@';
                        cur = up;
                    }
                    break;
            }

        }

        var boxes = map.map.Where(q => q.Value == 'O').ToList();

        foreach (var box in boxes)
        {
            sum += 100 * box.Key.y + box.Key.x;
        }

        return $"{sum}";
    }

    private bool TryMoveBox(CompassDirection dir, Coordinate2D cur, Coordinate2D next, bool partTwo = false)
    {
        var canmove = false;
        if(partTwo)
        {
            if (map2.map[next] == '.')
            {
                return true;
            }
            if (map2.map[next] == '[' || map2.map[next] == ']')
            {
                if(dir == CompassDirection.N || dir == CompassDirection.S)
                {
                    Coordinate2D otherpart;
                    if (map2.map[next] == '[')
                    {
                        otherpart = next.MoveDirection(CompassDirection.E);
                    }
                    else
                    {
                        otherpart = next.MoveDirection(CompassDirection.W);
                    }
                    boxesToMove.Add(new List<Coordinate2D> { next, otherpart });
                    canmove = TryMoveBox(dir, cur, otherpart.MoveDirection(dir), partTwo) &&
                        TryMoveBox(dir, cur, next.MoveDirection(dir), partTwo);
                }
                else
                {
                    var nextbox = next.MoveDirection(dir);
                    boxesToMove.Add(new List<Coordinate2D> { next, nextbox });
                    canmove = TryMoveBox(dir, cur, nextbox.MoveDirection(dir), partTwo);
                }
            }
            else if (map2.map[next] == '#')
            {
                return false;
            }
        }
        else
        {
            if (map.map[next] == '.')
            {
                map.map[cur] = '.';
                map.map[next] = 'O';
                return true;
            }
            else if (map.map[next] == 'O')
            {
                var nextbox = next.MoveDirection(dir);
                canmove = TryMoveBox(dir, cur, nextbox);
            }
            else if (map.map[next] == '#')
            {
                return false;
            }
            return canmove;
        }
        return canmove;
    }


    private string ProcessInput2()
    {
        long sum = 0;
        SetupMoves();
        var cur = map2.map.First(q => q.Value == '@').Key;
        CompassDirection movedir;
        while (Moves.TryDequeue(out char move))
        {
            movedir = move switch
            {
                '>' => CompassDirection.E,
                'v' => CompassDirection.N,
                '<' => CompassDirection.W,
                '^' => CompassDirection.S
            };

            var next = cur.MoveDirection(movedir);
            if (map2.map[next] == '[' || map2.map[next] == ']')
            {
                var canmove = TryMoveBox(movedir, cur, next, true);
                if (canmove)
                {
                    var edge1 = '[';
                    var edge2 = ']';
                    for (var x = boxesToMove.Count - 1; x >= 0; x--)
                    {
                        var curbox = boxesToMove[x];
                        var nextpos1 = curbox[0].MoveDirection(movedir);
                        var nextpos2 = curbox[1].MoveDirection(movedir);
                        if (nextpos1.x < nextpos2.x)
                        {
                            map2.map[nextpos1] = edge1;
                            map2.map[nextpos2] = edge2;
                        }
                        else
                        {
                            map2.map[nextpos1] = edge2;
                            map2.map[nextpos2] = edge1;
                        }
                        movedPositions.Add(nextpos1);
                        movedPositions.Add(nextpos2);
                    }

                    var newEmpty = boxesToMove.SelectMany(q => q.Where(e => !movedPositions.Contains(e))).ToList();

                    foreach(var empty in newEmpty)
                    {
                        map2.map[empty] = '.';
                    }

                    map2.map[cur] = '.';
                    map2.map[next] = '@';
                    cur = next;
                }
                movedPositions = new List<Coordinate2D>();
                boxesToMove = new List<List<Coordinate2D>>();
            }
            else if (map2.map[next] == '.')
            {
                map2.map[cur] = '.';
                map2.map[next] = '@';
                cur = next;
            }

        }

        var boxes = map2.map.Where(q => q.Value == '[').ToList();

        foreach (var box in boxes)
        {
            sum += 100 * box.Key.y + box.Key.x;
        }

        return $"{sum}";
    }

    public override ValueTask<string> Solve_1() => new(ProcessInput1());

    public override ValueTask<string> Solve_2() => new(ProcessInput2());
}
