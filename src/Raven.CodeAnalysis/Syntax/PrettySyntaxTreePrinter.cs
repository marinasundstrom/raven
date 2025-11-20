using System.IO;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax;

public class PrinterOptions
{
    public bool IncludeTokens { get; set; } = true;
    public bool IncludeTrivia { get; set; } = false;
    public bool IncludeSpans { get; set; } = false;
    public bool IncludeLocations { get; set; } = false;
    public bool IncludeNames { get; set; } = false;
    public bool Colorize { get; set; } = true;
    public int MaxDepth { get; set; } = int.MaxValue;
    public bool ExpandListsAsProperties { get; set; } = false;
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

        writer.WriteLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightBlue, printerOptions.Colorize) + value + MaybeColorize($"{node.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(node.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(node.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(node.GetLocation())}" : string.Empty)}");

        var newIndent = isFirst ? string.Empty : indent + (isLast ? IndentationStr : MarkerStraight);

        ChildSyntaxListItem[] children = node.ChildNodesAndTokens().ToArray();
        var printableChildren = GetPrintableChildren(children, printerOptions);

        for (int i = 0; i < printableChildren.Count; i++)
        {
            var printableChild = printableChildren[i];
            var isChildLast = i == printableChildren.Count - 1;

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

        writer.WriteLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{GetTokenText(ref token)} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(token.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}");

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

            writer.WriteLine($"{newIndent}{childMarker}" + $"{TriviaToString(trivia)}" + MaybeColorize($"{trivia.Kind}", AnsiColor.BrightRed, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(trivia.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(trivia.GetLocation())}" : string.Empty)}");

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
        writer.WriteLine($"{newIndent2}{MarkerBottom}" + MaybeColorize($"{name}{structure.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(structure.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(structure.GetLocation())}" : string.Empty)}");

        int i2 = 0;
        var structureChildren = structure.ChildNodesAndTokens();
        var structureCount = structureChildren.Count();
        foreach (var triviaChild in structureChildren)
        {
            var isChildLast2 = i2 == structureCount - 1;

            var newIndent4 = newIndent2 + IndentationStr;

            if (triviaChild.TryGetToken(out var token))
            {
                writer.WriteLine($"{newIndent4}{(isChildLast2 ? MarkerBottom : MarkerMiddle)}" + $"{token.Text} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}");
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


    private static string MaybeColorize(string text, AnsiColor color, bool colorize)
    {
        return colorize ? Colorize(text, color) : text;
    }

    private static string Colorize(string text, AnsiColor color)
    {
        return $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";
    }
}
