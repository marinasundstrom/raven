using System;
using System.Collections.Generic;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class SyntaxTreeProvider
{
    private static readonly HashSet<string> s_sourceExtensions = new(StringComparer.OrdinalIgnoreCase)
    {
        ".rvn",
        ".rav"
    };

    public static SyntaxTree? TryParse(string name, SourceText text, string? filePath = null)
    {
        var path = filePath ?? name;
        var ext = Path.GetExtension(path);
        if (!s_sourceExtensions.Contains(ext))
            return null;

        return SyntaxTree.ParseText(text, path: path);
    }
}
