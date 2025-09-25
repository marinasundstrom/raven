using System.Collections.Generic;
using System.Linq;
using System.Text;

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
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, true, printerOptions, printerOptions.MaxDepth);
        Console.WriteLine(sb.ToString());
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, PrinterOptions printerOptions)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, false, printerOptions, printerOptions.MaxDepth);
        return sb.ToString();
    }

    private const string IndentationStr = "    ";
    private const string MarkerTop = "┌── ";
    private const string MarkerStraight = "│   ";
    private const string MarkerMiddle = "├── ";
    private const string MarkerBottom = "└── ";

    private static void PrintSyntaxTreeCore(SyntaxNode node, StringBuilder sb, string indent, bool isFirst, bool isLast, PrinterOptions printerOptions, int currentDepth)
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

        sb.AppendLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightBlue, printerOptions.Colorize) + value + MaybeColorize($"{node.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(node.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(node.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(node.GetLocation())}" : string.Empty)}");

        var newIndent = isFirst ? string.Empty : indent + (isLast ? IndentationStr : MarkerStraight);

        ChildSyntaxListItem[] children = !printerOptions.IncludeTokens
            ? node.ChildNodesAndTokens().Where(x => x.IsNode).ToArray()
            : node.ChildNodesAndTokens().ToArray();

        Dictionary<SyntaxNodeOrToken, SyntaxNodeReflectionExtensions.ChildGroup>? listPropertyMap = null;
        Dictionary<string, (int FirstIndex, bool IsLast)>? listPropertyInfo = null;

        if (!printerOptions.ExpandListsAsProperties)
        {
            var grouped = node.GetChildrenGroupedByProperty(printerOptions.IncludeTokens);
            var listGroups = grouped.Properties.Where(p => p.IsList).ToList();

            if (listGroups.Count > 0)
            {
                listPropertyMap = new Dictionary<SyntaxNodeOrToken, SyntaxNodeReflectionExtensions.ChildGroup>();
                listPropertyInfo = new Dictionary<string, (int, bool)>();

                var orderedChildren = children.Select(c => (SyntaxNodeOrToken)c).ToArray();

                foreach (var group in listGroups)
                {
                    int firstIndex = int.MaxValue;
                    int lastIndex = -1;

                    foreach (var item in group.Items)
                    {
                        var childItem = item.Item;
                        listPropertyMap[childItem] = group;

                        var index = Array.IndexOf(orderedChildren, childItem);
                        if (index >= 0)
                        {
                            if (index < firstIndex)
                                firstIndex = index;

                            if (index > lastIndex)
                                lastIndex = index;
                        }
                    }

                    if (firstIndex != int.MaxValue)
                    {
                        bool groupIsLast = lastIndex == orderedChildren.Length - 1;
                        listPropertyInfo[group.PropertyName] = (firstIndex, groupIsLast);
                    }
                }
            }
        }

        for (int i = 0; i < children.Length; i++)
        {
            var isChildLast = i == children.Length - 1;
            var childNodeOrToken = (SyntaxNodeOrToken)children[i];

            if (!printerOptions.ExpandListsAsProperties && listPropertyMap is not null && listPropertyMap.TryGetValue(childNodeOrToken, out var listGroup))
            {
                if (listPropertyInfo is not null && listPropertyInfo.TryGetValue(listGroup.PropertyName, out var info))
                {
                    if (info.FirstIndex == i)
                    {
                        if (currentDepth + 1 <= printerOptions.MaxDepth)
                        {
                            PrintListProperty(sb, listGroup, newIndent, info.IsLast, printerOptions, currentDepth + 1);
                        }
                    }

                    continue;
                }
            }

            if (children[i].TryGetNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, newIndent, false, isChildLast, printerOptions, currentDepth + 1);
            }
            else if (printerOptions.IncludeTokens && children[i].TryGetToken(out var token))
            {
                PrintToken(sb, newIndent, isChildLast, printerOptions, ref token, currentDepth + 1);
            }
        }
    }

    private static void PrintListProperty(StringBuilder sb, SyntaxNodeReflectionExtensions.ChildGroup group, string indent, bool isLast, PrinterOptions printerOptions, int currentDepth)
    {
        if (currentDepth > printerOptions.MaxDepth)
            return;

        var marker = isLast ? MarkerBottom : MarkerMiddle;
        var propertyLabel = printerOptions.IncludeNames ? $"{group.PropertyName}: " : group.PropertyName;
        var propertyName = MaybeColorize(propertyLabel, AnsiColor.BrightBlue, printerOptions.Colorize);

        sb.AppendLine($"{indent}{marker}{propertyName}");

        var listIndent = indent + (isLast ? IndentationStr : MarkerStraight);

        var items = group.Items;
        for (int i = 0; i < items.Count; i++)
        {
            var item = items[i].Item;
            var isItemLast = i == items.Count - 1;

            if (item.TryGetNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, listIndent, false, isItemLast, printerOptions, currentDepth + 1);
            }
            else if (printerOptions.IncludeTokens && item.TryGetToken(out var token))
            {
                PrintToken(sb, listIndent, isItemLast, printerOptions, ref token, currentDepth + 1);
            }
        }
    }

    private static string GetTokenText(ref SyntaxToken token)
    {
        return token.Text
            .Replace("\r", "\\r")
            .Replace("\n", "\\n");
    }

    private static void PrintToken(StringBuilder sb, string indent, bool isChildLast, PrinterOptions printerOptions, ref SyntaxToken token, int currentDepth)
    {
        if (currentDepth > printerOptions.MaxDepth)
            return;

        if (printerOptions.IncludeTrivia)
        {
            var triviaList = token.LeadingTrivia;
            PrintTrivia(triviaList, true, sb, indent, false, isChildLast, printerOptions, currentDepth + 1);
        }

        var propertyName = string.Empty;

        if (printerOptions.IncludeNames)
        {
            propertyName = token.Parent?.GetPropertyNameForChild(token);

            if (propertyName is null && !printerOptions.ExpandListsAsProperties)
            {
                propertyName = token.Parent?.GetPropertyNameForListItem(token);
            }

            if (propertyName is not null)
            {
                propertyName = $"{propertyName}: ";
            }
        }

        string marker = isChildLast ? MarkerBottom : MarkerMiddle;

        sb.AppendLine($"{indent}{marker}" + MaybeColorize($"{propertyName}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{GetTokenText(ref token)} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(token.IsMissing ? " (Missing)" : string.Empty)}{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}");

        if (printerOptions.IncludeTrivia)
        {
            var triviaList = token.TrailingTrivia;
            PrintTrivia(triviaList, false, sb, indent, false, isChildLast, printerOptions, currentDepth + 1);
        }
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
            _ => string.Empty
        };
    }

    private static void PrintTrivia(SyntaxTriviaList triviaList, bool isLeading, StringBuilder sb, string indent, bool isFirst, bool isLast, PrinterOptions printerOptions, int currentDepth)
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

            sb.AppendLine($"{newIndent}{childMarker}" + $"{TriviaToString(trivia)}" + MaybeColorize($"{trivia.Kind}", AnsiColor.BrightRed, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(trivia.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(trivia.GetLocation())}" : string.Empty)}");

            if (trivia.HasStructure)
            {
                var newIndent2 = newIndent + MarkerStraight;

                PrintStructuredTrivia(sb, printerOptions, trivia, isFirstChild, isChildLast, newIndent2);
            }
        }
    }

    private static void PrintStructuredTrivia(StringBuilder sb, PrinterOptions printerOptions, SyntaxTrivia trivia, bool isFirstChild, bool isChildLast, string newIndent2)
    {
        string name = string.Empty;
        if (printerOptions.IncludeNames)
        {
            name = "Structure: ";
        }

        var structure = trivia.GetStructure()!;
        sb.AppendLine($"{newIndent2}{MarkerBottom}" + MaybeColorize($"{name}{structure.Kind}", AnsiColor.BrightBlue, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(structure.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(structure.GetLocation())}" : string.Empty)}");

        int i2 = 0;
        var structureChildren = structure.ChildNodesAndTokens();
        var structureCount = structureChildren.Count();
        foreach (var triviaChild in structureChildren)
        {
            var isChildLast2 = i2 == structureCount - 1;

            var newIndent4 = newIndent2 + IndentationStr;

            if (triviaChild.TryGetToken(out var token))
            {
                sb.AppendLine($"{newIndent4}{(isChildLast2 ? MarkerBottom : MarkerMiddle)}" + $"{token.Text} " + MaybeColorize($"{token.Kind}", AnsiColor.BrightGreen, printerOptions.Colorize) + $"{(printerOptions.IncludeSpans ? $" {Span(token.Span)}" : string.Empty)}{(printerOptions.IncludeLocations ? $" {Location(token.GetLocation())}" : string.Empty)}");
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
