using System;
using System.Threading.Tasks;

static class Program
{
    static async Task PrintDelayed(string label, int value)
    {
        await Task.Delay(10);
        Console.WriteLine($"{label}:{value}");
    }

    static async Task<int> Main(string[] args)
    {
        await PrintDelayed("first", 1);

        var second = await Task.FromResult(2);
        var third = await Task.FromResult(3);
        var total = 1 + second + third;
        Console.WriteLine($"sum:{total}");

        Console.WriteLine("done");
        return 0;
    }
}
