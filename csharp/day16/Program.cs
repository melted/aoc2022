using System.Collections.Immutable;
using System.Runtime.Intrinsics.X86;
using System.Text.RegularExpressions;

var data = File.ReadAllLines("D:/Niklas/repos/aoc2022/data/input16.txt");
Console.WriteLine(data.Length);

var valves = data.Select(d => ParseLine(d))
    .ToDictionary(v => v.name);

var flows = valves.Where(kv => kv.Value.flow > 0).ToImmutableDictionary();

var dists = new Dictionary<(string, string), int>();

foreach (var f in valves.Keys)
{
    foreach (var s in valves.Keys)
    {
        Distance(f, s);
    }
}

Console.WriteLine(FindMax(flows.Keys));
Console.WriteLine(FindMaxPart());

Valve ParseLine(string input) 
{
    string regex =
    @"Valve (\w*) has flow rate=(\d+); tunnels? leads? to valves? (.*)$";
    Regex rx = new Regex(regex);
    var match = rx.Match(input);
    var name = match.Groups[1].Value;
    var flow = match.Groups[2].Value;
    var tunnels = match.Groups[3].Value.Split(", ");
    return new Valve(name, Int32.Parse(flow), tunnels);
}

int Distance(string from, string to)
{
    if (dists.ContainsKey((from, to)))
    {
        return dists[(from, to)];
    }
    HashSet<string> visited = new();
    var dist = 0;
    HashSet<string> toCheck = new() { from };
    while(true)
    {
        if (toCheck.Any(s => s == to))
        {
            dists.Add((from, to), dist);
            return dist;
        } 
        else if (toCheck.Count == 0)
        {
            throw new Exception("Not connected");
        }
        dist++;
        visited.UnionWith(toCheck);
        var nextCheck = toCheck
            .SelectMany(s => valves[s].tunnels)
            .Where(p => !visited.Contains(p)).ToHashSet();
        toCheck = nextCheck;
    }
}

int FindMax(IEnumerable<string> nodes)
{
    var start = new Trip(ImmutableHashSet<string>.Empty, "AA", 4, 0);
    var trips = new Dictionary<ImmutableHashSet<string>, Trip>();
    var finished = new List<Trip>();
    trips.Add(ImmutableHashSet<string>.Empty, start);
    while(trips.Count > 0)
    {
        var todo = new List<Trip>();
        foreach (var trip in trips)
        {
            var next = NextTrips(trip.Value, nodes);
            if (next.Count == 0)
            {
                finished.Add(trip.Value);
                continue;
            }

            todo.AddRange(next);
        }
        trips.Clear();
        foreach (var t in todo)
        {
            if (!trips.ContainsKey(t.visited) || t.score > trips[t.visited].score)
            {
                trips[t.visited] = t;
            }
        }
    }

    return finished.Select(t => t.score).Max();
}

int FindMaxPart()
{
    int max = 0;
    var n = flows.Count; 
    for(uint i = 0; i < 2 << n; i++)
    {
        if (Math.Abs(Popcnt.PopCount(i) - n/2) <= 3)
        {
            var (a, b) = GetPartition(i, flows.Keys);
            var s1 = FindMax(a);
            var s2 = FindMax(b);
            if (s1 + s2 > max)
            {
                Console.WriteLine($"{i}: {s1+s2}");
                max = s1 + s2;   
            }
        }
    }
    return max;
}

(List<string>, List<string>) GetPartition(uint i, IEnumerable<string> nodes)
{
    var a = new List<string>();
    var b = new List<string>();
    foreach (var n in nodes)
    {
        if (i%2 == 0)
        {
            a.Add(n);
        } 
        else
        {
            b.Add(n);
        }
        i >>= 1;
    }

    return (a, b);
}

List<Trip> NextTrips(Trip trip, IEnumerable<string> nodes)
{
    var exits = nodes.ToImmutableHashSet().Remove(trip.current).Except(trip.visited);
    var res = new List<Trip>();
    foreach (var e in exits)
    {
        int flow = valves[e].flow;
        var t = trip.time+ dists[(trip.current, e)] + (flow > 0 ? 1 : 0);
        var newscore = !trip.visited.Contains(e) ? (30 - t) * flow : 0; 
        if (t <= 30)
        {
            var newTrip = new Trip(trip.visited.Add(e), e, t, trip.score + newscore);
            res.Add(newTrip);
        }
    }
    return res;
}

record Trip(ImmutableHashSet<string> visited, string current, int time, int score);

record Valve(string name, int flow, string[] tunnels);