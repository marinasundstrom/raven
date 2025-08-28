using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis.Text;

public static partial class AnsiExtensions
{
    private static readonly Regex s_ansiRegex = new("\u001B\\[[0-9;]*m", RegexOptions.Compiled);

    public static string StripAnsiCodes(this string text)
        => s_ansiRegex.Replace(text, string.Empty);
}

