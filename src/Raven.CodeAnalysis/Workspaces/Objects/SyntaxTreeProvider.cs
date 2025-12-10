using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides syntax tree parsing for documents. Only files with recognised Raven
/// source extensions are parsed; others return <c>null</c>.
/// </summary>
public class SyntaxTreeProvider
{
    private static readonly HashSet<string> s_sourceExtensions = new(RavenFileExtensions.All, StringComparer.OrdinalIgnoreCase);

    /// <summary>Determines whether the specified document supports a syntax tree.</summary>
    public virtual bool SupportsSyntaxTree(string name, string? filePath = null)
    {
        var path = filePath ?? name;
        var ext = Path.GetExtension(path);
        return s_sourceExtensions.Contains(ext);
    }

    /// <summary>Determines whether the specified document supports a semantic model.</summary>
    public virtual bool SupportsSemanticModel(string name, string? filePath = null)
        => SupportsSyntaxTree(name, filePath);

    /// <summary>
    /// Attempts to parse a <see cref="SyntaxTree"/> for the specified document
    /// name and text. Non-Raven files return <c>null</c>.
    /// </summary>
    public virtual SyntaxTree? TryParse(string name, SourceText text, string? filePath = null, TimeSpan? parseTimeout = null, CancellationToken cancellationToken = default)
    {
        if (!SupportsSyntaxTree(name, filePath))
            return null;

        var path = filePath ?? name;
        return SyntaxTree.ParseText(text, path: path, parseTimeout: parseTimeout, cancellationToken: cancellationToken);
    }
}
