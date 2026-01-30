using System.Diagnostics;

namespace Raven.CodeAnalysis.CodeGen;

public static class DebugUtils
{
    [Conditional("DEBUG")]
    public static void PrintDebug(string text)
    {
        if (CodeGenFlags.PrintDebug)
        {
            Console.WriteLine(text);
        }
    }
}
