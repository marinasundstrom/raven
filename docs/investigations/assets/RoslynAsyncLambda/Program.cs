using System;
using System.Threading.Tasks;

public static class Program
{
    public static async Task<int> Main(string[] args)
    {
        Func<int, Task<int>> compute = async value =>
        {
            await Task.Delay(5).ConfigureAwait(false);
            return value;
        };

        var result = await compute(42).ConfigureAwait(false);
        Console.WriteLine(result);
        return result;
    }
}
