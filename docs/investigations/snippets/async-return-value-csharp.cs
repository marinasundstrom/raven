using System;
using System.Threading.Tasks;

static class Program
{
    static async Task<int> Test(int value)
    {
        await Task.Delay(10);
        return value;
    }

    static async Task Main()
    {
        var x = await Test(42);
        Console.WriteLine(x);
    }
}
