using AoC2024;
using DotNetEnv;
using Microsoft.Extensions.DependencyInjection;


Env.Load();
string sessionCookie = Env.GetString("SESSION_COOKIE");
int year = int.Parse(Env.GetString("AOC_YEAR"));
int day = int.Parse(Env.GetString("AOC_DAY"));

var serviceProvider = new ServiceCollection()
            .AddSingleton<IDatasetReader>(provider => new DatasetReader(sessionCookie))
            .AddTransient<AdventSolver>()
            .BuildServiceProvider();

var solver = serviceProvider.GetRequiredService<AdventSolver>();
//solver.Solve(year, day, "samples/sample01_1.txt").Wait();
solver.Solve(year, day).Wait();
