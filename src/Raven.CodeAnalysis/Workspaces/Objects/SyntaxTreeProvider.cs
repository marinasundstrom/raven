using System;
using System.Collections.Generic;
using System.IO;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides syntax tree parsing for documents. Only files with recognised Raven
/// source extensions are parsed; others return <c>null</c>.
/// </summary>
public class SyntaxTreeProvider
{
    private static readonly HashSet<string> s_sourceExtensions = new(StringComparer.OrdinalIgnoreCase)
    {
        ".rvn",
        ".rav"
    };

    /// <summary>
    /// Attempts to parse a <see cref="SyntaxTree"/> for the specified document
    /// name and text. Non-Raven files return <c>null</c>.
    /// </summary>
    public virtual SyntaxTree? TryParse(string name, SourceText text, string? filePath = null)
    {
        var path = filePath ?? name;
        var ext = Path.GetExtension(path);
        if (!s_sourceExtensions.Contains(ext))
            return null;

        return SyntaxTree.ParseText(text, path: path);
    }
}
