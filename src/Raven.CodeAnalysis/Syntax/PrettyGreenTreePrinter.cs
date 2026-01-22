using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public sealed class PrettyGreenTreePrinterOptions
{
    public bool IncludeWidths { get; set; } = true;
    public bool IncludeDiagnostics { get; set; } = true;
    public bool IncludeAnnotations { get; set; } = false;
    public bool DiagnosticsAsChildren { get; set; } = false;
    public bool AnnotationsAsChildren { get; set; } = false;
    public bool IncludeSlotIndices { get; set; } = false;

    // When true, uses ANSI escape codes to colorize output for easier visual scanning.
    public bool Colorize { get; set; } = true;

    // When true, prints token leading/trailing trivia as children of the token.
    public bool IncludeTrivia { get; set; } = false;

    // When true, prints each trivia item text on the same line.
    public bool IncludeTriviaText { get; set; } = true;

    // When true, `List` nodes are not printed; their items are printed directly.
    public bool FlattenSyntaxLists { get; set; } = false;

    // When true, hides slot children whose node has Width == 0 and FullWidth == 0.
    // Slot indexing is preserved (original slot indices; flattened list items use "i.j").
    public bool HideZeroWidthSlots { get; set; } = false;
}

public static class PrettyGreenTreePrinter
{
    private const string Tee = "├── ";
    private const string Elbow = "└── ";
    private const string Pipe = "│   ";
    private const string Indent = "    ";

    public static void Print(GreenNode root, TextWriter writer, PrettyGreenTreePrinterOptions? options = null)
    {
        if (root is null) throw new ArgumentNullException(nameof(root));
        if (writer is null) throw new ArgumentNullException(nameof(writer));

        options ??= new PrettyGreenTreePrinterOptions();
        PrintNode(root, writer, options, indent: string.Empty, isLast: true, isRoot: true, slotIndex: null);
    }

    public static string PrintToString(GreenNode root, PrettyGreenTreePrinterOptions? options = null)
    {
        using var sw = new StringWriter();
        Print(root, sw, options);
        return sw.ToString();
    }

    private static void PrintNode(GreenNode node, TextWriter writer, PrettyGreenTreePrinterOptions options, string indent, bool isLast, bool isRoot, string? slotIndex)
    {
        // Print current line.
        if (isRoot)
        {
            writer.Write(indent);
            writer.Write(FormatGreenLine(node, options, slotIndex));
            writer.WriteLine();
        }
        else
        {
            writer.Write(indent);
            writer.Write(isLast ? Elbow : Tee);
            writer.Write(FormatGreenLine(node, options, slotIndex));
            writer.WriteLine();
        }

        // Compute indent for children.
        var nextIndent = indent + (isRoot ? string.Empty : (isLast ? Indent : Pipe));
        if (isRoot)
            nextIndent = indent + Indent;

        // Optional: diagnostics/annotations as their own arms under the node.
        var extraLines = new List<string>();
        if (options.IncludeDiagnostics && options.DiagnosticsAsChildren)
            extraLines.AddRange(DiagnosticsAsChildLines(node.GetDiagnostics(), options.Colorize));
        if (options.IncludeAnnotations && options.AnnotationsAsChildren)
            extraLines.AddRange(AnnotationsAsChildLines(node));

        // Determine whether there are "real" children after the extra lines.
        var hasTriviaChildren = options.IncludeTrivia && node is SyntaxToken tok2 && (tok2.LeadingTrivia.SlotCount > 0 || tok2.TrailingTrivia.SlotCount > 0);
        var slotChildren = GetSlotChildren(node, options);
        var hasSlotChildren = slotChildren.Count > 0;
        var hasRealChildren = hasTriviaChildren || hasSlotChildren;

        if (extraLines.Count > 0)
        {
            for (int i = 0; i < extraLines.Count; i++)
            {
                var isExtraLast = i == extraLines.Count - 1;
                // If there are real children, extra lines are never the last overall.
                if (hasRealChildren)
                    isExtraLast = false;

                writer.Write(nextIndent);
                writer.Write(isExtraLast ? Elbow : Tee);
                writer.WriteLine(extraLines[i]);
            }
        }

        // Trivia children for tokens (optional).
        if (options.IncludeTrivia && node is SyntaxToken tok)
        {
            var hasLeading = tok.LeadingTrivia.SlotCount > 0;
            var hasTrailing = tok.TrailingTrivia.SlotCount > 0;

            // Trivia lists are printed before slot children.
            // LeadingTrivia is last only if there is no TrailingTrivia and no slot children.
            // TrailingTrivia is last only if there are no slot children.
            if (hasLeading)
                PrintTriviaList("LeadingTrivia", tok.LeadingTrivia, writer, options, nextIndent, isLast: !hasTrailing && slotChildren.Count == 0);

            if (hasTrailing)
                PrintTriviaList("TrailingTrivia", tok.TrailingTrivia, writer, options, nextIndent, isLast: slotChildren.Count == 0);
        }

        // Slot children.
        for (int i = 0; i < slotChildren.Count; i++)
        {
            var (slotIndexText, child) = slotChildren[i];
            PrintNode(child, writer, options, nextIndent, isLast: i == slotChildren.Count - 1, isRoot: false, slotIndex: options.IncludeSlotIndices ? slotIndexText : null);
        }
    }

    private static List<(string SlotIndexText, GreenNode Node)> GetSlotChildren(GreenNode node, PrettyGreenTreePrinterOptions options)
    {
        var result = new List<(string SlotIndexText, GreenNode Node)>(capacity: Math.Max(0, node.SlotCount));

        bool ShouldHide(GreenNode n)
            => options.HideZeroWidthSlots && n.Width == 0 && n.FullWidth == 0;

        for (int i = 0; i < node.SlotCount; i++)
        {
            var child = node.GetSlot(i);
            if (child is null)
                continue;

            if (options.FlattenSyntaxLists && child.Kind == SyntaxKind.List)
            {
                // Expand the list's slots into the parent's effective child list.
                for (int j = 0; j < child.SlotCount; j++)
                {
                    var item = child.GetSlot(j);
                    if (item is null)
                        continue;

                    if (ShouldHide(item))
                        continue;

                    result.Add(($"{i}.{j}", item));
                }

                continue;
            }

            if (ShouldHide(child))
                continue;

            result.Add(($"{i}", child));
        }

        return result;
    }

    private static void PrintTriviaList(string name, SyntaxTriviaList triviaList, TextWriter writer, PrettyGreenTreePrinterOptions options, string indent, bool isLast)
    {
        if (triviaList.SlotCount == 0)
            return;

        // Header line for the trivia list.
        writer.Write(indent);
        writer.Write(isLast ? Elbow : Tee);
        writer.WriteLine(name);

        var nextIndent = indent + (isLast ? Indent : Pipe);
        for (int i = 0; i < triviaList.SlotCount; i++)
        {
            var trivia = triviaList[i];
            var isLastItem = i == triviaList.SlotCount - 1;

            writer.Write(nextIndent);
            writer.Write(isLastItem ? Elbow : Tee);
            writer.Write(FormatTriviaLine(trivia, options));
            writer.WriteLine();

            // Structured trivia: print its structure as a green node if available.
            if (trivia.HasStructuredTrivia)
            {
                var structure = trivia.GetStructuredTrivia();
                if (structure is not null)
                {
                    var structIndent = nextIndent + (isLastItem ? Indent : Pipe);
                    PrintNode(structure, writer, options, structIndent, isLast: true, isRoot: false, slotIndex: null);
                }
            }
        }
    }

    private static string FormatGreenLine(GreenNode node, PrettyGreenTreePrinterOptions options, string? slotIndex)
    {
        var prefix = slotIndex is not null ? $"[{slotIndex}] " : string.Empty;

        // For tokens, include value text like: IdentifierToken: System
        var kind = MaybeColorize(node.Kind.ToString(), AnsiColor.BrightGreen, options.Colorize);
        var valueText = node.GetValueText();
        var valuePart = string.Empty;
        if (!string.IsNullOrEmpty(valueText) && valueText != kind)
            valuePart = $": {Escape(valueText)}";

        var missingPart = node.IsMissing ? " (Missing)" : string.Empty;
        var widthPart = options.IncludeWidths ? $" [W={node.Width}, FW={node.FullWidth}]" : string.Empty;
        var diagPart = (options.IncludeDiagnostics && !options.DiagnosticsAsChildren) ? DiagnosticsToString(node.GetDiagnostics(), options.Colorize) : string.Empty;
        var annPart = (options.IncludeAnnotations && !options.AnnotationsAsChildren) ? AnnotationsToString(node) : string.Empty;

        return prefix + kind + valuePart + missingPart + widthPart + diagPart + annPart;
    }

    private static string FormatTriviaLine(SyntaxTrivia trivia, PrettyGreenTreePrinterOptions options)
    {
        var kind = MaybeColorize(trivia.Kind.ToString(), AnsiColor.BrightRed, options.Colorize);
        var textPart = string.Empty;

        if (options.IncludeTriviaText && !trivia.HasStructuredTrivia)
        {
            var text = trivia.Text
                .Replace("\r", "\\r", StringComparison.Ordinal)
                .Replace("\n", "\\n", StringComparison.Ordinal)
                .Replace("\t", "\\t", StringComparison.Ordinal)
                .Replace(" ", "␣", StringComparison.Ordinal);
            textPart = $": {text}";
        }

        var diagPart = options.IncludeDiagnostics ? DiagnosticsToString(trivia.GetDiagnostics(), options.Colorize) : string.Empty;
        return kind + textPart + diagPart;
    }

    private static string Escape(string s)
        => s.Replace("\r", "\\r", StringComparison.Ordinal)
            .Replace("\n", "\\n", StringComparison.Ordinal)
            .Replace("\t", "\\t", StringComparison.Ordinal);

    private static string AnnotationsToString(GreenNode node)
    {
        // GreenNode stores annotations internally; print them compactly.
        var annotations = node._annotations;
        if (annotations is null || annotations.Length == 0)
            return string.Empty;

        var kinds = annotations
            .Select(a => a.Kind)
            .Where(k => !string.IsNullOrWhiteSpace(k))
            .Distinct()
            .ToList();

        if (kinds.Count == 0)
            return string.Empty;

        return " " + string.Join(" ", kinds.Select(k => $"[@{k}]"));
    }

    private static IEnumerable<string> DiagnosticsAsChildLines(IEnumerable<DiagnosticInfo> diagnostics, bool colorize)
    {
        var list = diagnostics?.ToList();
        if (list is null || list.Count == 0)
            return Array.Empty<string>();

        return list.Select(d =>
        {
            var id = ColorizeDiagnosticId(d, colorize);
            var msg = GetDiagnosticMessage(d);

            string sev;
            try
            {
                sev = d.Descriptor?.DefaultSeverity.ToString() ?? string.Empty;
            }
            catch
            {
                sev = string.Empty;
            }

            var sevPart = string.IsNullOrWhiteSpace(sev) ? string.Empty : $" {sev}";
            return string.IsNullOrWhiteSpace(msg) ? $"{id}{sevPart}" : $"{id}{sevPart}: {msg}";
        });
    }

    private static IEnumerable<string> AnnotationsAsChildLines(GreenNode node)
    {
        var annotations = node._annotations;
        if (annotations is null || annotations.Length == 0)
            return Array.Empty<string>();

        return annotations.Select(a =>
        {
            var kind = a.Kind;
            var data = a.Data;
            return data is null ? $"[@{kind}]" : $"[@{kind}] = {data}";
        });
    }

    private static string? GetDiagnosticMessage(DiagnosticInfo diagnostic)
    {
        try
        {
            var format = diagnostic.Descriptor?.MessageFormat;
            if (string.IsNullOrWhiteSpace(format))
                return null;

            var args = diagnostic.Args;
            if (args is null || args.Length == 0)
                return format;

            return string.Format(format!, args);
        }
        catch
        {
            // If formatting fails (bad format string/arg mismatch), fall back to the raw format.
            try
            {
                return diagnostic.Descriptor?.MessageFormat;
            }
            catch
            {
                return null;
            }
        }
    }

    private static string DiagnosticsToString(IEnumerable<DiagnosticInfo> diagnostics, bool colorize)
    {
        if (diagnostics is null)
            return string.Empty;

        var list = diagnostics.ToList();
        if (list.Count == 0)
            return string.Empty;

        // Keep stable ordering by first occurrence, but remove duplicates by id.
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var parts = new List<string>();
        foreach (var d in list)
        {
            var id = d.Descriptor?.Id;
            if (string.IsNullOrWhiteSpace(id))
                continue;

            if (!seen.Add(id))
                continue;

            parts.Add(ColorizeDiagnosticId(d, colorize));
        }

        if (parts.Count == 0)
            return string.Empty;

        return " " + string.Join(" ", parts);
    }

    private static string ColorizeDiagnosticId(DiagnosticInfo diagnostic, bool colorize)
    {
        var id = diagnostic.Descriptor?.Id;
        var text = string.IsNullOrWhiteSpace(id) ? "[?]" : $"[{id}]";

        DiagnosticSeverity? sev = null;
        try { sev = diagnostic.Descriptor?.DefaultSeverity; } catch { }

        var color = sev switch
        {
            DiagnosticSeverity.Error => AnsiColor.BrightRed,
            DiagnosticSeverity.Warning => AnsiColor.Yellow,
            DiagnosticSeverity.Info => AnsiColor.Cyan,
            _ => AnsiColor.BrightBlue,
        };

        return colorize ? Colorize(text, color) : text;
    }

    private enum AnsiColor
    {
        BrightRed = 91,
        BrightGreen = 92,
        Yellow = 33,
        Cyan = 36,
        BrightBlue = 94,
        Reset = 0,
    }

    private static string MaybeColorize(string text, AnsiColor color, bool colorize)
        => colorize ? Colorize(text, color) : text;

    private static string Colorize(string text, AnsiColor color)
        => $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";
}
