using System;
using System.Xml.Schema;

internal class Day06 : IDay
{
    public async Task<int> Part1(List<string> data)
    {
        return -1;
        var tiles = data.Select((d, dy) => d.Select((d, dx) => new Tile { x = dx, y = dy, isObstruciton = d == '#', visited = d == '^' })).SelectMany(t => t).ToList();
        var ybound = data.Count;
        var xbound = data.First().Length;
        var obs = tiles.Where(t => t.isObstruciton).ToList();

        var guard = tiles.Where(t => t.visited).First();
        var (dirx, diry) = (0, -1);
        bool outside = false;

        var (gx, gy) = (guard.x, guard.y);
        do
        {
            var gTile = tiles.Where((t) => t.x == gx && t.y == gy).First();
            gTile.visited = true;
            
            var nextTile = tiles.Where((t) => t.x == gx + dirx && t.y == gy + diry).First();
            if (nextTile.isObstruciton)
            {
                if (diry == -1) { dirx = 1; diry = 0; }
                else if (diry == 1) { dirx = -1; diry = 0; }
                else if (dirx == -1) { dirx = 0; diry = -1; }
                else if (dirx == 1) { dirx = 0; diry = 1; }
            } else
            {
                gx += dirx;
                gy += diry;
            }
            outside = gx + dirx >= xbound || gy + diry >= ybound || gx + dirx < 0 || gy + diry < 0;

        } while (!outside);

        return tiles.Where(t => t.visited == true).Count() + 1; //visitedList.Count + 1;
    }

    public async Task<int> Part2(List<string> data)
    {
        var tiles = data.Select((d, dy) => d.Select((d, dx) => new Tile { x = dx, y = dy, isObstruciton = d == '#', visited = d == '^', valid = true })).SelectMany(t => t).ToList();
        var ybound = data.Count;
        var xbound = data.First().Length;
        var obs = tiles.Where(t => t.isObstruciton).ToList();

        var guard = tiles.Where(t => t.visited).First();
        var (dirx, diry) = (0, -1);
        bool outside = false;

        var (gx, gy) = (guard.x, guard.y);
        do
        {
            var gTile = tiles.Where((t) => t.x == gx && t.y == gy).First();
            gTile.visited = true;

            var nextTile = tiles.Where((t) => t.x == gx + dirx && t.y == gy + diry).First();
            if (nextTile.isObstruciton)
            {
                if (diry == -1) { dirx = 1; diry = 0; }
                else if (diry == 1) { dirx = -1; diry = 0; }
                else if (dirx == -1) { dirx = 0; diry = -1; }
                else if (dirx == 1) { dirx = 0; diry = 1; }
            }
            else
            {
                gx += dirx;
                gy += diry;
            }
            outside = gx + dirx >= xbound || gy + diry >= ybound || gx + dirx < 0 || gy + diry < 0;

        } while (!outside);

        var vcoutn = tiles.Where(t => t.visited).Count();
        int count = 0;
        foreach (var obsTile in tiles.Where(t => !t.isObstruciton))
        {
            var cornerUp = obs.Where(o => o.x == obsTile.x + 1 && o.y < obsTile.y).OrderByDescending(o => o.y).FirstOrDefault();
            var cornerRight = obs.Where(o => o.y == obsTile.y + 1 && o.x > obsTile.x).OrderBy(o => o.x).FirstOrDefault();
            var cornerDown = obs.Where(o => o.x == obsTile.x - 1 && o.y > obsTile.y).OrderBy(o => o.y).FirstOrDefault();
            var cornerLeft = obs.Where(o => o.y == obsTile.y - 1 && o.x < obsTile.x).OrderByDescending(o => o.x).FirstOrDefault();
            if (cornerUp != null && cornerRight != null)
            {
                var oppositeCorner = obs.Where(o => o.x == cornerRight.x + 1 && o.y == cornerUp.y + 1).FirstOrDefault();
                if (oppositeCorner != null)
                {
                    var t = tiles.Where(t => t.x == obsTile.x + 1 && t.y == obsTile.y).First();
                    count += t.visited ? 1 : 0;
                }
                //count += oppositeCorner != null ? 1 : 0;
            }
            if (cornerRight != null && cornerDown != null)
            {
                var oppositeCorner = obs.Where(o => o.x == cornerRight.x - 1 && o.y == cornerDown.y + 1).FirstOrDefault();
                if (oppositeCorner != null)
                {
                    var t = tiles.Where(t => t.x == obsTile.x && t.y == obsTile.y + 1).First();
                    count += t.visited ? 1 : 0;
                }
                //count += oppositeCorner != null ? 1 : 0;
            }
            if (cornerDown != null && cornerLeft != null)
            {
                var oppositeCorner = obs.Where(o => o.x == cornerLeft.x - 1 && o.y == cornerDown.y - 1).FirstOrDefault();
                if (oppositeCorner != null)
                {
                    var t = tiles.Where(t => t.x == obsTile.x - 1 && t.y == obsTile.y).First();
                    count += t.visited ? 1 : 0;
                }
                //count += oppositeCorner != null ? 1 : 0;
            }
            if (cornerLeft != null && cornerUp != null)
            {
                var oppositeCorner = obs.Where(o => o.x == cornerLeft.x + 1 && o.y == cornerUp.y - 1).FirstOrDefault();
                if (oppositeCorner != null)
                {
                    var t = tiles.Where(t => t.x == obsTile.x && t.y == obsTile.y - 1).First();
                    count += t.visited ? 1 : 0;
                }
                //count += oppositeCorner != null ? 1 : 0;
            }
        }
        
        return count;
    }

    class Tile
    {
        public int x { get; set; }
        public int y { get; set; }
        public bool isObstruciton { get; set; }
        public bool visited { get; set; }
        public bool valid { get; set; }
    }
}