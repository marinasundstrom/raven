using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroSyntaxOrigin
{
    private const string AuthoredOriginKind = "MacroAuthoredOrigin";
    private const string HiddenExpansionKind = "MacroHiddenExpansion";

    public static TSyntax AttachParsedOrigin<TSyntax>(TSyntax syntax, SyntaxTree? sourceTree)
        where TSyntax : SyntaxNode
    {
        if (sourceTree is null || string.IsNullOrWhiteSpace(sourceTree.FilePath))
            return syntax;

        return Rewrite(
            syntax,
            span => span.Length == 0 ? null : span,
            sourceTree.FilePath);
    }

    public static TSyntax AttachBodyOrigin<TSyntax>(
        TSyntax syntax,
        SyntaxTree? sourceTree,
        TextSpan bodySpan,
        TextSpan bodyRelativeSpan)
        where TSyntax : SyntaxNode
    {
        if (sourceTree is null || string.IsNullOrWhiteSpace(sourceTree.FilePath))
            return syntax;

        var authoredSpan = new TextSpan(
            bodySpan.Start + bodyRelativeSpan.Start,
            bodyRelativeSpan.Length);
        return Rewrite(
            syntax,
            span => span.Length == 0 ? null : authoredSpan,
            sourceTree.FilePath);
    }

    public static TSyntax AttachMappedOrigins<TSyntax>(
        TSyntax syntax,
        SyntaxTree? sourceTree,
        TextSpan bodySpan,
        IReadOnlyList<MacroExpansionSourceMap> sourceMaps)
        where TSyntax : SyntaxNode
    {
        if (sourceTree is null || string.IsNullOrWhiteSpace(sourceTree.FilePath) || sourceMaps.Count == 0)
            return syntax;

        return Rewrite(syntax, MapSpan, sourceTree.FilePath);

        TextSpan? MapSpan(TextSpan expandedNodeSpan)
        {
            if (expandedNodeSpan.Length == 0)
                return null;

            MacroExpansionSourceMap? best = null;
            foreach (var sourceMap in sourceMaps)
            {
                if (!sourceMap.ExpandedSpan.Contains(expandedNodeSpan))
                    continue;

                if (best is null || sourceMap.ExpandedSpan.Length < best.Value.ExpandedSpan.Length)
                    best = sourceMap;
            }

            if (best is not { } mapping)
                return null;

            var relativeStart = expandedNodeSpan.Start - mapping.ExpandedSpan.Start;
            if (relativeStart < 0 || relativeStart + expandedNodeSpan.Length > mapping.BodyRelativeSpan.Length)
                return null;

            return new TextSpan(
                bodySpan.Start + mapping.BodyRelativeSpan.Start + relativeStart,
                expandedNodeSpan.Length);
        }
    }

    public static TSyntax MarkGeneratedSyntaxHidden<TSyntax>(
        TSyntax syntax,
        SyntaxNode authoredRoot)
        where TSyntax : SyntaxNode
    {
        var authoredGreens = new HashSet<GreenNode>(ReferenceEqualityComparer.Instance);
        foreach (var node in authoredRoot.DescendantNodesAndSelf())
            authoredGreens.Add(node.Green);
        return (TSyntax)RewriteHiddenGreen(syntax.Green, authoredGreens)
            .CreateRed(parent: null, position: syntax.Position);
    }

    public static bool IsHidden(SyntaxNode syntax)
        => syntax.GetAnnotation(HiddenExpansionKind) is not null;

    public static bool ContainsAuthoredOrigin(SyntaxNode syntax)
        => syntax.DescendantNodesAndSelf().Any(
            static node => node.GetAnnotation(AuthoredOriginKind) is not null);

    public static bool TryGetFirstAuthoredSourceSpan(
        SyntaxNode syntax,
        Compilation compilation,
        out SyntaxTree sourceTree,
        out TextSpan sourceSpan)
    {
        foreach (var candidate in syntax.DescendantNodesAndSelf())
        {
            if (TryGetSourceSpan(candidate, compilation, out sourceTree, out sourceSpan))
                return true;
        }

        sourceTree = null!;
        sourceSpan = default;
        return false;
    }

    public static bool TryGetSourceSpan(
        SyntaxNode syntax,
        Compilation compilation,
        out SyntaxTree sourceTree,
        out TextSpan sourceSpan)
    {
        sourceTree = null!;
        sourceSpan = default;

        var annotation = syntax.GetAnnotation(AuthoredOriginKind);
        if (annotation?.Data is not { } data || !TryDecode(data, out var filePath, out sourceSpan))
            return false;

        sourceTree = compilation.SyntaxTrees.FirstOrDefault(
            tree => PathsEqual(tree.FilePath, filePath))!;
        return sourceTree is not null && sourceSpan.End <= sourceTree.GetText().Length;
    }

    public static bool TryGetSourceSpan(
        SyntaxNode syntax,
        SyntaxTree expectedSourceTree,
        out TextSpan sourceSpan)
    {
        sourceSpan = default;

        var annotation = syntax.GetAnnotation(AuthoredOriginKind);
        return annotation?.Data is { } data &&
            TryDecode(data, out var filePath, out sourceSpan) &&
            PathsEqual(expectedSourceTree.FilePath, filePath) &&
            sourceSpan.End <= expectedSourceTree.GetText().Length;
    }

    private static TSyntax Rewrite<TSyntax>(
        TSyntax syntax,
        Func<TextSpan, TextSpan?> mapSpan,
        string filePath)
        where TSyntax : SyntaxNode
    {
        var rewrittenGreen = RewriteOriginGreen(syntax.Green, syntax.Position, mapSpan, filePath);
        return (TSyntax)rewrittenGreen.CreateRed(parent: null, position: syntax.Position);
    }

    private static GreenNode RewriteOriginGreen(
        GreenNode green,
        int position,
        Func<TextSpan, TextSpan?> mapSpan,
        string filePath)
    {
        var children = new GreenNode[green.SlotCount];
        var changed = false;
        for (var index = 0; index < green.SlotCount; index++)
        {
            var child = green.GetSlot(index);
            if (child is null)
                continue;

            var rewrittenChild = RewriteOriginGreen(
                child,
                position + green.GetChildStartPosition(index),
                mapSpan,
                filePath);
            children[index] = rewrittenChild;
            changed |= !ReferenceEquals(child, rewrittenChild);
        }

        var rewritten = changed
            ? green.With(children, green._diagnostics, green._annotations)
            : green;
        if (green is not Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode)
            return rewritten;

        var span = new TextSpan(position + green.GetLeadingTriviaWidth(), green.Width);
        return mapSpan(span) is { } sourceSpan
            ? rewritten.WithAdditionalAnnotations(CreateAnnotation(filePath, sourceSpan))
            : rewritten;
    }

    private static GreenNode RewriteHiddenGreen(
        GreenNode green,
        HashSet<GreenNode> authoredGreens)
    {
        if (authoredGreens.Contains(green))
            return green;

        var children = new GreenNode[green.SlotCount];
        var changed = false;
        for (var index = 0; index < green.SlotCount; index++)
        {
            var child = green.GetSlot(index);
            if (child is null)
                continue;

            var rewrittenChild = RewriteHiddenGreen(child, authoredGreens);
            children[index] = rewrittenChild;
            changed |= !ReferenceEquals(child, rewrittenChild);
        }

        var rewritten = changed
            ? green.With(children, green._diagnostics, green._annotations)
            : green;
        if (green is not Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode ||
            rewritten.GetAnnotation(AuthoredOriginKind) is not null)
        {
            return rewritten;
        }

        return rewritten.WithAdditionalAnnotations(new SyntaxAnnotation(HiddenExpansionKind));
    }

    private static SyntaxAnnotation CreateAnnotation(string filePath, TextSpan span)
    {
        var encodedPath = Convert.ToBase64String(Encoding.UTF8.GetBytes(Path.GetFullPath(filePath)));
        return new SyntaxAnnotation(AuthoredOriginKind, $"{encodedPath}:{span.Start}:{span.Length}");
    }

    private static bool TryDecode(string data, out string filePath, out TextSpan span)
    {
        filePath = string.Empty;
        span = default;
        var parts = data.Split(':');
        if (parts.Length != 3 ||
            !int.TryParse(parts[1], out var start) ||
            !int.TryParse(parts[2], out var length) ||
            start < 0 ||
            length < 0)
        {
            return false;
        }

        try
        {
            filePath = Encoding.UTF8.GetString(Convert.FromBase64String(parts[0]));
            span = new TextSpan(start, length);
            return true;
        }
        catch (FormatException)
        {
            return false;
        }
    }

    private static bool PathsEqual(string? left, string right)
    {
        if (string.IsNullOrWhiteSpace(left))
            return false;

        return string.Equals(
            Path.GetFullPath(left),
            Path.GetFullPath(right),
            OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal);
    }
}
