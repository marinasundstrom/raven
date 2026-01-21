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

    // When true, prints token leading/trailing trivia as children of the token.
    public bool IncludeTrivia { get; set; } = false;

    // When true, prints each trivia item text on the same line.
    public bool IncludeTriviaText { get; set; } = true;

    // When true, `List` nodes are not printed; their items are printed directly.
    public bool FlattenSyntaxLists { get; set; } = false;
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

    private static void PrintNode(GreenNode node, TextWriter writer, PrettyGreenTreePrinterOptions options, string indent, bool isLast, bool isRoot, int? slotIndex)
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
            extraLines.AddRange(DiagnosticsAsChildLines(node.GetDiagnostics()));
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
            PrintTriviaList("LeadingTrivia", tok.LeadingTrivia, writer, options, nextIndent);
            PrintTriviaList("TrailingTrivia", tok.TrailingTrivia, writer, options, nextIndent);
        }

        // Slot children.
        for (int i = 0; i < slotChildren.Count; i++)
        {
            PrintNode(slotChildren[i], writer, options, nextIndent, isLast: i == slotChildren.Count - 1, isRoot: false, slotIndex: options.IncludeSlotIndices ? i : null);
        }
    }

    private static List<GreenNode> GetSlotChildren(GreenNode node, PrettyGreenTreePrinterOptions options)
    {
        var result = new List<GreenNode>(capacity: Math.Max(0, node.SlotCount));

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
                    result.Add(item);
                }

                continue;
            }

            result.Add(child);
        }

        return result;
    }

    private static void PrintTriviaList(string name, SyntaxTriviaList triviaList, TextWriter writer, PrettyGreenTreePrinterOptions options, string indent)
    {
        if (triviaList.SlotCount == 0)
            return;

        // Header line for the trivia list.
        writer.Write(indent);
        writer.Write(Tee);
        writer.WriteLine(name);

        var nextIndent = indent + Pipe;
        for (int i = 0; i < triviaList.SlotCount; i++)
        {
            var trivia = triviaList[i];
            var isLast = i == triviaList.SlotCount - 1;

            writer.Write(nextIndent);
            writer.Write(isLast ? Elbow : Tee);
            writer.Write(FormatTriviaLine(trivia, options));
            writer.WriteLine();

            // Structured trivia: print its structure as a green node if available.
            if (trivia.HasStructuredTrivia)
            {
                var structure = trivia.GetStructuredTrivia();
                if (structure is not null)
                {
                    var structIndent = nextIndent + (isLast ? Indent : Pipe);
                    PrintNode(structure, writer, options, structIndent, isLast: true, isRoot: false, slotIndex: null);
                }
            }
        }
    }

    private static string FormatGreenLine(GreenNode node, PrettyGreenTreePrinterOptions options, int? slotIndex)
    {
        var prefix = slotIndex is int idx ? $"[{idx}] " : string.Empty;

        // For tokens, include value text like: IdentifierToken: System
        var kind = node.Kind.ToString();
        var valueText = node.GetValueText();
        var valuePart = string.Empty;
        if (!string.IsNullOrEmpty(valueText) && valueText != kind)
            valuePart = $": {Escape(valueText)}";

        var missingPart = node.IsMissing ? " (Missing)" : string.Empty;
        var widthPart = options.IncludeWidths ? $" [W={node.Width}, FW={node.FullWidth}]" : string.Empty;
        var diagPart = (options.IncludeDiagnostics && !options.DiagnosticsAsChildren) ? DiagnosticsToString(node.GetDiagnostics()) : string.Empty;
        var annPart = (options.IncludeAnnotations && !options.AnnotationsAsChildren) ? AnnotationsToString(node) : string.Empty;

        return prefix + kind + valuePart + missingPart + widthPart + diagPart + annPart;
    }

    private static string FormatTriviaLine(SyntaxTrivia trivia, PrettyGreenTreePrinterOptions options)
    {
        var kind = trivia.Kind.ToString();
        var textPart = string.Empty;

        if (options.IncludeTriviaText && !trivia.HasStructuredTrivia)
        {
            var text = trivia.Text
                .Replace("\r", "\\r", StringComparison.Ordinal)
                .Replace("\n", "\\n", StringComparison.Ordinal)
                .Replace("\t", "\\t", StringComparison.Ordinal);
            textPart = $": {text}";
        }

        var diagPart = options.IncludeDiagnostics ? DiagnosticsToString(trivia.GetDiagnostics()) : string.Empty;
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

    private static IEnumerable<string> DiagnosticsAsChildLines(IEnumerable<DiagnosticInfo> diagnostics)
    {
        var list = diagnostics?.ToList();
        if (list is null || list.Count == 0)
            return Array.Empty<string>();

        return list.Select(d =>
        {
            var id = d.Descriptor.Id;
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
            return string.IsNullOrWhiteSpace(msg) ? $"[{id}]{sevPart}" : $"[{id}]{sevPart}: {msg}";
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

    private static string DiagnosticsToString(IEnumerable<DiagnosticInfo> diagnostics)
    {
        if (diagnostics is null)
            return string.Empty;

        var ids = diagnostics
            .Select(d => d.Descriptor.Id)
            .Where(id => !string.IsNullOrWhiteSpace(id))
            .Distinct()
            .ToList();

        if (ids.Count == 0)
            return string.Empty;

        return " " + string.Join(" ", ids.Select(id => $"[{id}]"));
    }
}
