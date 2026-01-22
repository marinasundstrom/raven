using System.IO;
using System;
using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public class PrinterOptions
{
    /// <summary>
    /// When true, prints token children (SyntaxToken) in the tree. When false, tokens are omitted.
    /// </summary>
    public bool IncludeTokens { get; set; } = true;

    /// <summary>
    /// When true, prints leading/trailing trivia for tokens (whitespace, comments, newlines, etc.).
    /// </summary>
    public bool IncludeTrivia { get; set; } = false;

    /// <summary>
    /// When true, appends each node/token/trivia span (TextSpan) to the printed line.
    /// </summary>
    public bool IncludeSpans { get; set; } = false;

    /// <summary>
    /// When true, appends each node/token/trivia location (line/column range) to the printed line.
    /// </summary>
    public bool IncludeLocations { get; set; } = false;

    /// <summary>
    /// When true, prints property names (e.g. "Condition:") for children when available.
    /// </summary>
    public bool IncludeNames { get; set; } = false;

    /// <summary>
    /// When true, uses ANSI escape codes to colorize output for easier visual scanning.
    /// </summary>
    public bool Colorize { get; set; } = true;

    /// <summary>
    /// Maximum recursion depth to print. Nodes deeper than this are not printed.
    /// </summary>
    public int MaxDepth { get; set; } = int.MaxValue;

    /// <summary>
    /// When true, groups list items under a single property node (instead of printing each list item as a separate child).
    /// </summary>
    public bool ExpandListsAsProperties { get; set; } = false;

    /// <summary>
    /// When true, prints diagnostics attached to nodes/tokens/trivia.
    /// </summary>
    public bool IncludeDiagnostics { get; set; } = false;

    /// <summary>
    /// When true, prints annotations attached to nodes/tokens/trivia.
    /// </summary>
    public bool IncludeAnnotations { get; set; } = false;

    /// <summary>
    /// When true, diagnostics are printed as separate child lines under the owning node (instead of inline on the same line).
    /// </summary>
    public bool DiagnosticsAsChildren { get; set; } = false;

    /// <summary>
    /// When true, annotations are printed as separate child lines under the owning node (instead of inline on the same line).
    /// </summary>
    public bool AnnotationsAsChildren { get; set; } = false;
}

public static class PrettySyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, PrinterOptions printerOptions)
    {
        Console.WriteLine(node.GetSyntaxTreeRepresentation(printerOptions));
    }

    public static void PrintSyntaxTree(this SyntaxNode node, PrinterOptions printerOptions, TextWriter writer)
    {
        PrintSyntaxTreeCore(node, writer, string.Empty, true, true, printerOptions, printerOptions.MaxDepth);
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, PrinterOptions printerOptions)
    {
        using var writer = new StringWriter();
        PrintSyntaxTreeCore(node, writer, string.Empty, true, false, printerOptions, printerOptions.MaxDepth);
        return writer.ToString();
    }

    private const string IndentationStr = "    ";
    private const string MarkerTop = "┌── ";
    private const string MarkerStraight = "│   ";
    private const string MarkerMiddle = "├── ";
    private const string MarkerBottom = "└── ";

    private static void PrintSyntaxTreeCore(SyntaxNode node, TextWriter writer, string indent, bool isFirst, bool isLast, PrinterOptions printerOptions, int currentDepth)
    {
        if (currentDepth > printerOptions.MaxDepth)
            return;

        // Visual markers for tree structure
        string? marker;
        if (isLast && !isFirst)
        {
            marker = MarkerBottom;
        }
        else
        {
            if (isFirst)
            {
                marker = string.Empty;
            }
            else
            {
                marker = MarkerMiddle;
            }
        }

        var propertyName = string.Empty;

        if (printerOptions.IncludeNames)
        {
            propertyName = node.Parent?.GetPropertyNameForChild(node);

            if (propertyName is not null)
            {
                propertyName = $"{propertyName}: ";
            }
        }

        var value = !printerOptions.IncludeTokens ? MaybeColorize(Value(node), AnsiColor.Yellow, printerOptions.Colorize) : string.Empty;

        var diagnosticsStr = (printerOptions.IncludeDiagnostics && !printerOptions.DiagnosticsAsChildren) ? DiagnosticsToString(node.GetDiagnostics(false), printerOptions.Colorize) : string.Empty;
        var annotationsStr = (printerOptions.IncludeAnnotations && !printerOptions.AnnotationsAsChildren) ? AnnotationsToString(node) : string.Empty;

        writer.WriteLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightBlue, printerOptions.Colorize) + value + MaybeColorize($"{node.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(node.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(node.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(node.GetLocation())}" : string.Empty)}{diagnosticsStr}{annotationsStr}");

        var newIndent = isFirst ? string.Empty : indent + (isLast ? IndentationStr : MarkerStraight);

        ChildSyntaxListItem[] children = node.ChildNodesAndTokens().ToArray();
        var printableChildren = GetPrintableChildren(children, printerOptions);

        // Optional: print diagnostics/annotations as their own arms under the node.
        var diagChildren = (printerOptions.IncludeDiagnostics && printerOptions.DiagnosticsAsChildren)
            ? DiagnosticsAsChildLines(node.GetDiagnostics(false))
            : Array.Empty<string>();

        var annChildren = (printerOptions.IncludeAnnotations && printerOptions.AnnotationsAsChildren)
            ? AnnotationsAsChildLines(node)
            : Array.Empty<string>();

        var extraChildren = diagChildren.Concat(annChildren).ToList();

        if (extraChildren.Count > 0)
        {
            var total = extraChildren.Count + printableChildren.Count;

            for (int i = 0; i < extraChildren.Count; i++)
            {
                var isExtraLast = (i == total - 1) && printableChildren.Count == 0;
                // If there are real children, extra items are never the last overall unless there are none.
                if (printableChildren.Count > 0)
                    isExtraLast = false;

                var childMarker = isExtraLast ? MarkerBottom : MarkerMiddle;
                writer.WriteLine($"{newIndent}{childMarker}{extraChildren[i]}");
            }

            // If there are real children, ensure the last marker alignment remains correct by treating
            // extra children as preceding siblings.
        }

        var extraCount = (printerOptions.IncludeDiagnostics && printerOptions.DiagnosticsAsChildren ? diagChildren.Length : 0)
            + (printerOptions.IncludeAnnotations && printerOptions.AnnotationsAsChildren ? annChildren.Length : 0);

        for (int i = 0; i < printableChildren.Count; i++)
        {
            var printableChild = printableChildren[i];
            var isChildLast = i == printableChildren.Count - 1;
            // Note: isChildLast is correct because extra children are printed before real children.

            if (printableChild.IsList)
            {
                PrintListProperty(printableChild, writer, newIndent, isChildLast, printerOptions, currentDepth + 1);
            }
            else if (printableChild.Item is { } child && child.TryGetNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, writer, newIndent, false, isChildLast, printerOptions, currentDepth + 1);
            }
            else if (printerOptions.IncludeTokens && printableChild.Item is { } tokenChild && tokenChild.TryGetToken(out var token))
            {
                PrintToken(token, writer, newIndent, isChildLast, printerOptions, currentDepth + 1);
            }
        }
    }

    private static IReadOnlyList<PrintableChild> GetPrintableChildren(ChildSyntaxListItem[] children, PrinterOptions printerOptions)
    {
        var result = new List<PrintableChild>();

        if (!printerOptions.ExpandListsAsProperties && printerOptions.IncludeNames)
        {
            var listPropertyIndices = new Dictionary<string, List<int>>();

            for (int i = 0; i < children.Length; i++)
            {
                var child = children[i];

                if (!printerOptions.IncludeTokens && child.IsToken)
                    continue;

                if (child.ParentListGreen is null)
                    continue;

                var propertyName = GetListPropertyName(child, printerOptions);

                if (propertyName is null)
                    continue;

                if (!listPropertyIndices.TryGetValue(propertyName, out var indices))
                {
                    indices = new List<int>();
                    listPropertyIndices[propertyName] = indices;
                }

                indices.Add(i);
            }

            var handled = new HashSet<int>();

            for (int i = 0; i < children.Length; i++)
            {
                if (!printerOptions.IncludeTokens && children[i].IsToken)
                    continue;

                if (handled.Contains(i))
                    continue;

                var child = children[i];

                var propertyName = GetListPropertyName(child, printerOptions);

                if (propertyName is not null && listPropertyIndices.TryGetValue(propertyName, out var indices) && indices.Count > 0 && indices[0] == i)
                {
                    var items = indices
                        .Select(index => children[index])
                        .Where(item => printerOptions.IncludeTokens || item.IsNode)
                        .ToArray();

                    foreach (var index in indices)
                        handled.Add(index);

                    if (items.Length > 0)
                        result.Add(PrintableChild.ForList(propertyName, items));

                    continue;
                }

                handled.Add(i);
                result.Add(PrintableChild.ForSingle(child));
            }

            return result;
        }

        foreach (var child in children)
        {
            if (!printerOptions.IncludeTokens && child.IsToken)
                continue;

            result.Add(PrintableChild.ForSingle(child));
        }

        return result;
    }

    private static string? GetListPropertyName(ChildSyntaxListItem child, PrinterOptions printerOptions)
    {
        if (child.ParentListGreen is null)
            return null;

        if (child.TryGetNode(out var childNode))
            return childNode.Parent?.GetPropertyNameForListItem(childNode);

        if (printerOptions.IncludeTokens && child.TryGetToken(out var token))
            return token.Parent?.GetPropertyNameForListItem(token);

        return null;
    }

    private static void PrintListProperty(PrintableChild printableChild, TextWriter writer, string indent, bool isChildLast, PrinterOptions printerOptions, int currentDepth)
    {
        if (currentDepth > printerOptions.MaxDepth)
            return;

        var marker = isChildLast ? MarkerBottom : MarkerMiddle;
        writer.WriteLine($"{indent}{marker}" + MaybeColorize(printableChild.ListPropertyName!, AnsiColor.BrightBlue, printerOptions.Colorize));

        var childIndent = indent + (isChildLast ? IndentationStr : MarkerStraight);

        var items = printableChild.ListItems!;

        for (int i = 0; i < items.Count; i++)
        {
            var listChild = items[i];
            var isListChildLast = i == items.Count - 1;

            if (listChild.TryGetNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, writer, childIndent, false, isListChildLast, printerOptions, currentDepth + 1);
            }
            else if (printerOptions.IncludeTokens && listChild.TryGetToken(out var token))
            {
                PrintToken(token, writer, childIndent, isListChildLast, printerOptions, currentDepth + 1);
            }
        }
    }

    private static void PrintToken(SyntaxToken token, TextWriter writer, string indent, bool isChildLast, PrinterOptions printerOptions, int currentDepth)
    {
        if (currentDepth > printerOptions.MaxDepth)
            return;

        if (printerOptions.IncludeTrivia)
        {
            var triviaList = token.LeadingTrivia;
            PrintTrivia(triviaList, true, writer, indent, false, isChildLast, printerOptions, currentDepth + 1);
        }

        var propertyName = string.Empty;

        if (printerOptions.IncludeNames)
        {
            propertyName = token.Parent?.GetPropertyNameForChild(token);

            if (propertyName is not null)
            {
                propertyName = $"{propertyName}: ";
            }
        }

        var marker = isChildLast ? MarkerBottom : MarkerMiddle;

        var diagnosticsStr = (printerOptions.IncludeDiagnostics && !printerOptions.DiagnosticsAsChildren) ? DiagnosticsToString(token.GetDiagnostics(), printerOptions.Colorize) : string.Empty;
        var annotationsStr = printerOptions.IncludeAnnotations ? AnnotationsToString(token) : string.Empty;

        writer.WriteLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{GetTokenText(ref token)} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(token.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}{diagnosticsStr}{annotationsStr}");

        if (printerOptions.IncludeTrivia)
        {
            var triviaList = token.TrailingTrivia;
            PrintTrivia(triviaList, false, writer, indent, false, isChildLast, printerOptions, currentDepth + 1);
        }
    }

    private sealed class PrintableChild
    {
        private PrintableChild(ChildSyntaxListItem item)
        {
            Item = item ?? throw new ArgumentNullException(nameof(item));
        }

        private PrintableChild(string propertyName, IReadOnlyList<ChildSyntaxListItem> items)
        {
            ListPropertyName = propertyName ?? throw new ArgumentNullException(nameof(propertyName));
            ListItems = items ?? throw new ArgumentNullException(nameof(items));
        }

        public ChildSyntaxListItem? Item { get; }

        public IReadOnlyList<ChildSyntaxListItem>? ListItems { get; }

        public string? ListPropertyName { get; }

        public bool IsList => ListItems is not null;

        public static PrintableChild ForSingle(ChildSyntaxListItem item) => new(item);

        public static PrintableChild ForList(string propertyName, IReadOnlyList<ChildSyntaxListItem> items) => new(propertyName, items);
    }

    private static string GetTokenText(ref SyntaxToken token)
    {
        return token.Text
            .Replace("\r", "\\r")
            .Replace("\n", "\\n");
    }

    private static string Value(SyntaxNode node)
    {
        return node switch
        {
            UnaryExpressionSyntax ue => $"{ue.OperatorToken.Text} ",
            BinaryExpressionSyntax be => $"{be.OperatorToken.Text} ",
            LiteralExpressionSyntax { Kind: SyntaxKind.NullLiteralExpression } le => $"null ",
            LiteralExpressionSyntax { Kind: SyntaxKind.StringLiteralExpression } le => $"{le.Token.Text} ",
            LiteralExpressionSyntax { Kind: SyntaxKind.TrueLiteralExpression } le => $"{le.Token.Text} ",
            LiteralExpressionSyntax { Kind: SyntaxKind.FalseKeyword } le => $"{le.Token.Text} ",
            LiteralExpressionSyntax le => $"{le.Token.Value} ",
            IdentifierNameSyntax ine => $"{ine.Identifier.Text} ",
            PredefinedTypeSyntax pt => $"{pt.Keyword.Text} ",
            UnitTypeSyntax _ => "() ",
            FunctionTypeSyntax ft => $"{ft} ",
            DefaultExpressionSyntax { Type: { } type } => $"default({type}) ",
            DefaultExpressionSyntax => "default ",
            TypeOfExpressionSyntax typeOf => $"typeof({typeOf.Type}) ",
            _ => string.Empty
        };
    }

    private static void PrintTrivia(SyntaxTriviaList triviaList, bool isLeading, TextWriter writer, string indent, bool isFirst, bool isLast, PrinterOptions printerOptions, int currentDepth)
    {
        var newIndent = isFirst ? string.Empty : indent + (isLast ? (isLeading ? MarkerStraight : IndentationStr) : MarkerStraight);

        var listCount = triviaList.Count;
        for (int i = 0; i < listCount; i++)
        {
            var trivia = triviaList[i];
            var isFirstChild = i == 0;
            bool isChildLast = i == (listCount - 1);

            string childMarker = null;

            if (listCount == 0)
            {
                childMarker = isLeading ? MarkerTop : MarkerBottom;
            }
            else
            {
                if (isLeading)
                {
                    childMarker = isFirstChild ? MarkerTop : MarkerMiddle;
                }
                else
                {
                    childMarker = isChildLast ? MarkerBottom : MarkerMiddle;
                }
            }

            var diagnosticsStr = printerOptions.IncludeDiagnostics ? DiagnosticsToString(GetDiagnostics(trivia), printerOptions.Colorize) : string.Empty;
            var annotationsStr = printerOptions.IncludeAnnotations ? AnnotationsToString(trivia) : string.Empty;
            writer.WriteLine($"{newIndent}{childMarker}" + $"{TriviaToString(trivia)}" + MaybeColorize($"{trivia.Kind}", AnsiColor.BrightRed, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(trivia.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(trivia.GetLocation())}" : string.Empty)}{diagnosticsStr}{annotationsStr}");

            if (trivia.HasStructure)
            {
                var newIndent2 = newIndent + MarkerStraight;

                PrintStructuredTrivia(writer, printerOptions, trivia, isFirstChild, isChildLast, newIndent2);
            }
        }
    }

    private static void PrintStructuredTrivia(TextWriter writer, PrinterOptions printerOptions, SyntaxTrivia trivia, bool isFirstChild, bool isChildLast, string newIndent2)
    {
        string name = string.Empty;
        if (printerOptions.IncludeNames)
        {
            name = "Structure: ";
        }

        var structure = trivia.GetStructure()!;
        var diagnosticsStr = printerOptions.IncludeDiagnostics ? DiagnosticsToString(structure.GetDiagnostics(), printerOptions.Colorize) : string.Empty;
        var annotationsStr = printerOptions.IncludeAnnotations ? AnnotationsToString(structure) : string.Empty;
        writer.WriteLine($"{newIndent2}{MarkerBottom}" + MaybeColorize($"{name}{structure.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(structure.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(structure.GetLocation())}" : string.Empty)}{diagnosticsStr}{annotationsStr}");

        int i2 = 0;
        var structureChildren = structure.ChildNodesAndTokens();
        var structureCount = structureChildren.Count();
        foreach (var triviaChild in structureChildren)
        {
            var isChildLast2 = i2 == structureCount - 1;

            var newIndent4 = newIndent2 + IndentationStr;

            if (triviaChild.TryGetToken(out var token))
            {
                var diagnosticsStr2 = printerOptions.IncludeDiagnostics ? DiagnosticsToString(token.GetDiagnostics(), printerOptions.Colorize) : string.Empty;
                var annotationsStr2 = printerOptions.IncludeAnnotations ? AnnotationsToString(token) : string.Empty;
                writer.WriteLine($"{newIndent4}{(isChildLast2 ? MarkerBottom : MarkerMiddle)}" + $"{token.Text} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}{diagnosticsStr2}{annotationsStr2}");
            }
            i2++;
        }
    }

    private static string Location(Location location)
    {
        var lineSpan = location.GetLineSpan();
        var start = lineSpan.StartLinePosition;
        var end = lineSpan.EndLinePosition;
        return $"({start.Line + 1},{start.Character + 1}) - ({end.Line + 1},{end.Character + 1})";
    }

    private static string Span(TextSpan span)
    {
        return span.ToString();
    }

    private static string TriviaToString(SyntaxTrivia trivia)
    {
        if (trivia.HasStructure)
            return string.Empty;

        return trivia.ToString()
            .Replace("\r", @"\r")
            .Replace("\n", @"\n")
            .Replace("\t", @"\t")
            .Replace(" ", "␣") + " ";

        // ␠
    }

    private static IEnumerable<Diagnostic> GetDiagnostics(object syntax)
    {
        // Be defensive: different syntax types may expose diagnostics APIs differently.
        try
        {
            var type = syntax.GetType();

            // Common: GetDiagnostics() => IEnumerable<Diagnostic>
            var m0 = type.GetMethod("GetDiagnostics", Type.EmptyTypes);
            if (m0 != null)
            {
                var result = m0.Invoke(syntax, null);
                if (result is IEnumerable<Diagnostic> diags)
                    return diags;
            }

            // Alternative: GetDiagnostics(bool includeSuppressed)
            var m1 = type.GetMethod("GetDiagnostics", new[] { typeof(bool) });
            if (m1 != null)
            {
                var result = m1.Invoke(syntax, new object?[] { false });
                if (result is IEnumerable<Diagnostic> diags)
                    return diags;
            }
        }
        catch
        {
            // ignore
        }

        return Array.Empty<Diagnostic>();
    }

    private static string DiagnosticsToString(IEnumerable<Diagnostic> diagnostics, bool colorize)
    {
        var list = diagnostics?.ToList();
        if (list is null || list.Count == 0)
            return string.Empty;

        return " " + string.Join(", ", list.Select(d => ColorizeDiagnosticId(d, colorize)));
    }

    private static string ColorizeDiagnosticId(Diagnostic diagnostic, bool colorize)
    {
        var text = $"[{diagnostic.Id}]";

        // Severity-based coloring:
        // - Error   => Red
        // - Warning => Yellow
        // - Info    => Cyan
        var color = diagnostic.Severity switch
        {
            DiagnosticSeverity.Error => AnsiColor.BrightRed,
            DiagnosticSeverity.Warning => AnsiColor.Yellow,
            DiagnosticSeverity.Info => AnsiColor.Cyan,
            _ => AnsiColor.BrightBlue,
        };

        return colorize ? Colorize(text, color) : text;
    }

    private static string MaybeColorize(string text, AnsiColor color, bool colorize)
    {
        return colorize ? Colorize(text, color) : text;
    }

    private static string Colorize(string text, AnsiColor color)
    {
        return $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";
    }

    private static string AnnotationsToString(object syntax)
    {
        // Be defensive: annotation APIs may differ between node/token/trivia in the red tree.
        // We try common shapes via reflection and fall back to empty.
        try
        {
            var type = syntax.GetType();

            // Common pattern: GetAnnotations() => IEnumerable<SyntaxAnnotation>
            var m0 = type.GetMethod("GetAnnotations", Type.EmptyTypes);
            if (m0 != null)
            {
                var result = m0.Invoke(syntax, null);
                if (result is System.Collections.IEnumerable enumerable)
                {
                    var kinds = enumerable
                        .Cast<object>()
                        .Select(a => a?.GetType().GetProperty("Kind")?.GetValue(a) as string)
                        .Where(k => !string.IsNullOrWhiteSpace(k))
                        .Distinct()
                        .ToList();

                    if (kinds.Count > 0)
                        return " " + string.Join(", ", kinds.Select(k => $"[@{k}]"));
                }
            }

            // Alternative: HasAnnotations + GetAnnotations(string kind) isn't enumerable without knowing kinds.
            // If the object exposes an internal field `_annotations`, use it.
            var f = type.GetField("_annotations", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            if (f?.GetValue(syntax) is Array arr && arr.Length > 0)
            {
                var kinds = arr
                    .Cast<object>()
                    .Select(a => a?.GetType().GetProperty("Kind")?.GetValue(a) as string)
                    .Where(k => !string.IsNullOrWhiteSpace(k))
                    .Distinct()
                    .ToList();

                if (kinds.Count > 0)
                    return " " + string.Join(", ", kinds.Select(k => $"[@{k}]"));
            }
        }
        catch
        {
            // ignore
        }

        return string.Empty;
    }

    private static string[] DiagnosticsAsChildLines(IEnumerable<Diagnostic> diagnostics)
    {
        var list = diagnostics?.ToList();
        if (list is null || list.Count == 0)
            return Array.Empty<string>();

        return list
            .Select(d => $"[{d.Id}]: {d.GetMessage()}")
            .ToArray();
    }

    private static string[] AnnotationsAsChildLines(object syntax)
    {
        var pairs = GetAnnotationPairs(syntax);
        if (pairs.Count == 0)
            return Array.Empty<string>();

        return pairs
            .Select(p => p.data is null ? $"[@{p.kind}]" : $"[@{p.kind}] = {p.data}")
            .ToArray();
    }

    private static List<(string kind, string? data)> GetAnnotationPairs(object syntax)
    {
        var result = new List<(string kind, string? data)>();

        try
        {
            var type = syntax.GetType();

            // Prefer GetAnnotations() => IEnumerable<SyntaxAnnotation>
            var m0 = type.GetMethod("GetAnnotations", Type.EmptyTypes);
            if (m0 != null)
            {
                var ann = m0.Invoke(syntax, null);
                if (ann is System.Collections.IEnumerable enumerable)
                {
                    foreach (var a in enumerable)
                    {
                        if (a is null) continue;
                        var at = a.GetType();
                        var kind = at.GetProperty("Kind")?.GetValue(a) as string;
                        if (string.IsNullOrWhiteSpace(kind)) continue;
                        var data = at.GetProperty("Data")?.GetValue(a)?.ToString();
                        result.Add((kind!, data));
                    }
                }

                return result
                    .Where(x => !string.IsNullOrWhiteSpace(x.kind))
                    .Distinct()
                    .ToList();
            }

            // Fallback: internal field `_annotations`
            var f = type.GetField("_annotations", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public);
            if (f?.GetValue(syntax) is Array arr && arr.Length > 0)
            {
                foreach (var a in arr)
                {
                    if (a is null) continue;
                    var at = a.GetType();
                    var kind = at.GetProperty("Kind")?.GetValue(a) as string;
                    if (string.IsNullOrWhiteSpace(kind)) continue;
                    var data = at.GetProperty("Data")?.GetValue(a)?.ToString();
                    result.Add((kind!, data));
                }
            }
        }
        catch
        {
            // ignore
        }

        return result
            .Where(x => !string.IsNullOrWhiteSpace(x.kind))
            .Distinct()
            .ToList();
    }
}
