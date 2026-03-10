using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal sealed class SourceDiagnosticSuppressionMap
{
    private static readonly char[] DirectiveSeparators = [',', ' ', '\t'];
    private readonly ImmutableArray<Directive> _directives;
    private readonly SourceText _sourceText;

    private SourceDiagnosticSuppressionMap(SourceText sourceText, ImmutableArray<Directive> directives)
    {
        _sourceText = sourceText;
        _directives = directives;
    }

    public static SourceDiagnosticSuppressionMap Create(SyntaxTree tree)
    {
        var root = tree.GetRoot();
        var sourceText = tree.GetText() ?? SourceText.From(root.ToFullString());

        var directives = ImmutableArray.CreateBuilder<Directive>();
        foreach (var trivia in root.DescendantTrivia())
            CollectDirectives(trivia, sourceText, directives);

        directives.Sort(static (left, right) =>
        {
            var lineComparison = left.Line.CompareTo(right.Line);
            return lineComparison != 0 ? lineComparison : left.Order.CompareTo(right.Order);
        });

        return new SourceDiagnosticSuppressionMap(sourceText, directives.ToImmutable());
    }

    public bool IsSuppressed(Diagnostic diagnostic)
    {
        if (_directives.IsDefaultOrEmpty)
            return false;

        var sourceTree = diagnostic.Location.SourceTree;
        if (sourceTree is null)
            return false;

        var line = _sourceText.GetLineAndColumn(diagnostic.Location.SourceSpan.Start).line;

        var suppressAll = false;
        var suppressedIds = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var restoredIds = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var directive in _directives)
        {
            if (directive.Line > line)
                break;

            if (directive.AppliesToAll)
            {
                if (directive.Disable)
                {
                    suppressAll = true;
                    restoredIds.Clear();
                }
                else
                {
                    suppressAll = false;
                    suppressedIds.Clear();
                    restoredIds.Clear();
                }

                continue;
            }

            foreach (var id in directive.DiagnosticIds)
            {
                if (directive.Disable)
                {
                    if (suppressAll)
                        restoredIds.Remove(id);
                    else
                        suppressedIds.Add(id);
                }
                else
                {
                    if (suppressAll)
                        restoredIds.Add(id);
                    else
                        suppressedIds.Remove(id);
                }
            }
        }

        if (suppressAll)
            return !restoredIds.Contains(diagnostic.Id);

        return suppressedIds.Contains(diagnostic.Id);
    }

    private static void CollectDirectives(SyntaxTrivia trivia, SourceText sourceText, ImmutableArray<Directive>.Builder directives)
    {
        if (trivia.Kind is not SyntaxKind.SingleLineCommentTrivia
            and not SyntaxKind.MultiLineCommentTrivia
            and not SyntaxKind.DirectiveTrivia)
            return;

        var startLine = sourceText.GetLineAndColumn(trivia.SpanStart).line;
        var lines = GetDirectiveLines(trivia.Text);
        for (var lineIndex = 0; lineIndex < lines.Length; lineIndex++)
        {
            var line = lines[lineIndex];
            if (TryParseDirective(line, out var action, out var appliesToAll, out var diagnosticIds))
            {
                var directiveLine = startLine + lineIndex;
                switch (action)
                {
                    case DirectiveAction.Disable:
                        directives.Add(new Directive(directiveLine, directives.Count, Disable: true, appliesToAll, diagnosticIds));
                        break;
                    case DirectiveAction.Restore:
                        directives.Add(new Directive(directiveLine, directives.Count, Disable: false, appliesToAll, diagnosticIds));
                        break;
                    case DirectiveAction.DisableNextLine:
                        directives.Add(new Directive(directiveLine + 1, directives.Count, Disable: true, appliesToAll, diagnosticIds));
                        directives.Add(new Directive(directiveLine + 2, directives.Count, Disable: false, appliesToAll, diagnosticIds));
                        break;
                }
            }
        }
    }

    private static ImmutableArray<string> GetDirectiveLines(string commentText)
    {
        if (string.IsNullOrWhiteSpace(commentText))
            return ImmutableArray<string>.Empty;

        if (commentText.StartsWith("//", StringComparison.Ordinal))
            return [commentText[2..]];

        if (commentText.StartsWith('#'))
            return [commentText[1..]];

        if (commentText.StartsWith("/*", StringComparison.Ordinal))
        {
            var content = commentText[2..];
            if (content.EndsWith("*/", StringComparison.Ordinal))
                content = content[..^2];

            var split = content
                .Replace("\r\n", "\n", StringComparison.Ordinal)
                .Replace('\r', '\n')
                .Split('\n')
                .Select(static line => TrimBlockCommentLinePrefix(line))
                .ToImmutableArray();

            return split;
        }

        return ImmutableArray<string>.Empty;
    }

    private static string TrimBlockCommentLinePrefix(string line)
    {
        var trimmed = line.TrimStart();
        if (trimmed.StartsWith('*'))
            trimmed = trimmed[1..];

        return trimmed;
    }

    private static bool TryParseDirective(
        string lineText,
        out DirectiveAction action,
        out bool appliesToAll,
        out ImmutableArray<string> diagnosticIds)
    {
        action = DirectiveAction.Restore;
        appliesToAll = false;
        diagnosticIds = ImmutableArray<string>.Empty;

        if (string.IsNullOrWhiteSpace(lineText))
            return false;

        var trimmed = lineText.Trim();
        if (trimmed.StartsWith("raven ", StringComparison.OrdinalIgnoreCase))
            trimmed = trimmed["raven ".Length..].TrimStart();

        if (!trimmed.StartsWith("pragma warning ", StringComparison.OrdinalIgnoreCase))
            return false;

        var commandAndIds = trimmed["pragma warning ".Length..].TrimStart();
        if (commandAndIds.StartsWith("disable-next-line", StringComparison.OrdinalIgnoreCase))
        {
            action = DirectiveAction.DisableNextLine;
            commandAndIds = commandAndIds["disable-next-line".Length..];
        }
        else if (commandAndIds.StartsWith("disable", StringComparison.OrdinalIgnoreCase))
        {
            action = DirectiveAction.Disable;
            commandAndIds = commandAndIds["disable".Length..];
        }
        else if (commandAndIds.StartsWith("restore", StringComparison.OrdinalIgnoreCase))
        {
            action = DirectiveAction.Restore;
            commandAndIds = commandAndIds["restore".Length..];
        }
        else
        {
            return false;
        }

        var ids = commandAndIds
            .Split(DirectiveSeparators, StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(static id => id.TrimEnd(';'))
            .Where(static id => !string.IsNullOrWhiteSpace(id))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

        if (ids.Length == 0 || ids.Contains("*", StringComparer.Ordinal))
        {
            appliesToAll = true;
            diagnosticIds = ImmutableArray<string>.Empty;
            return true;
        }

        diagnosticIds = ids;
        return true;
    }

    private readonly record struct Directive(
        int Line,
        int Order,
        bool Disable,
        bool AppliesToAll,
        ImmutableArray<string> DiagnosticIds);

    private enum DirectiveAction
    {
        Disable,
        Restore,
        DisableNextLine
    }
}
