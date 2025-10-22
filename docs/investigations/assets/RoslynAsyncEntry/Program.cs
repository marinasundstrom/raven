using System;
using System.Threading.Tasks;

internal static class Program
{
    private static async Task<int> Main()
    {
        var value = await Test(42);
        Console.WriteLine(value);
        return 0;
    }

    private static async Task<int> Test(int value)
    {
        await Task.Delay(10);
        return value;
    }
}
