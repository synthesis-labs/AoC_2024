internal class Day06 : IDay
{
    private readonly Tuple<int, int> _results;
    public Day06(List<string> data)
    {
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
            }
            else
            {
                nextTile.visited = true;
                gx += dirx;
                gy += diry;
            }
            outside = gx + dirx >= xbound || gy + diry >= ybound || gx + dirx < 0 || gy + diry < 0;

        } while (!outside);

        int count = 0;
        int currentIdx = 0;
        foreach (var placedObs in tiles.Where(t => t.visited).ToList())
        {
            currentIdx++;
            foreach (var item in tiles)
            {
                item.bottomHit = 0;
                item.leftHit = 0;
                item.rightHit = 0;
                item.topHit = 0;
            }
            bool noloop = true;
            bool inside = true;
            placedObs.isObstruciton = true;
            (gx, gy) = (guard.x, guard.y);
            (dirx, diry) = (0, -1);

            do
            {
                var gTile = tiles.Where((t) => t.x == gx && t.y == gy).First();
                var nextTile = tiles.Where((t) => t.x == gx + dirx && t.y == gy + diry).First();
                if (nextTile.isObstruciton)
                {
                    if (diry == -1) { dirx = 1; diry = 0; nextTile.bottomHit++; noloop = noloop && nextTile.bottomHit < 2; }
                    else if (diry == 1) { dirx = -1; diry = 0; nextTile.topHit++; noloop = noloop && nextTile.topHit < 2; }
                    else if (dirx == -1) { dirx = 0; diry = -1; nextTile.rightHit++; noloop = noloop && nextTile.rightHit < 2; }
                    else if (dirx == 1) { dirx = 0; diry = 1; nextTile.leftHit++; noloop = noloop && nextTile.leftHit < 2; }
                }
                else
                {
                    gx += dirx;
                    gy += diry;
                }
                inside = !(gx + dirx >= xbound || gy + diry >= ybound || gx + dirx < 0 || gy + diry < 0);

            } while (inside && noloop);
            count += noloop ? 0 : 1;
            placedObs.isObstruciton = false;
        }
        _results = new Tuple<int, int>(tiles.Where(t => t.visited).Count(), count);
    }

    public double Part1(List<string> data) => _results.Item1;

    public double Part2(List<string> data) => _results.Item2;

    class Tile
    {
        public int x { get; set; }
        public int y { get; set; }
        public bool isObstruciton { get; set; }
        public bool visited { get; set; }
        public int topHit { get; set; }
        public int bottomHit { get; set; }
        public int leftHit { get; set; }
        public int rightHit { get; set; }
    }
}