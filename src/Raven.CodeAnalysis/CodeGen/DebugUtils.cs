using System.Diagnostics;

namespace Raven.CodeAnalysis.CodeGen;

public static class DebugUtils
{
    [Conditional("DEBUG")]
    public static void PrintDebug(string text)
    {
        if (CodeGenFlags.PrintDebug)
        {
            PrintCore(text);
        }
    }

    [Conditional("DEBUG")]
    public static void PrintDebug(string text, Func<bool> condition)
    {
        if (!CodeGenFlags.PrintDebug)
            return;

        if (!condition())
            return;

        PrintCore(text);
    }

    private static void PrintCore(string text)
    {
        Console.WriteLine(text);
    }
}
