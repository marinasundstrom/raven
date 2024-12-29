using System.Drawing;
using System.Runtime.CompilerServices;
using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static class PrettySyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, bool includeTokens = true, bool includeTrivia = false, bool includeSpans = false, bool includeLocation = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, true, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth);
        Console.WriteLine(sb.ToString());
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, bool includeTokens = true, bool includeTrivia = false, bool includeSpans = false, bool includeLocation = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, false, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth);
        return sb.ToString();
    }

    private const string IndentationStr = "    ";
    private const string MarkerTop = "┌── ";
    private const string MarkerStraight = "│   ";
    private const string MarkerMiddle = "├── ";
    private const string MarkerBottom = "└── ";

    private static void PrintSyntaxTreeCore(SyntaxNode node, StringBuilder sb, string indent, bool isFirst, bool isLast, bool includeTokens, bool includeTrivia, bool includeSpans, bool includeLocation, int maxDepth, int currentDepth = 0)
    {
        if (currentDepth > maxDepth)
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

        sb.AppendLine($"{indent}{marker}" + Colorize($"{node.Kind}", ConsoleColor.Blue) + $"{(includeSpans ? $" {Span(node.Span)}" : string.Empty)}{(includeLocation ? $" {Location(node.GetLocation())}" : string.Empty)}");

        var newIndent = isFirst ? string.Empty : indent + (isLast ? IndentationStr : MarkerStraight);

        var children = node.ChildNodesAndTokens().ToArray();

        if (!includeTokens)
        {
            children = children.Where(x => x.IsNode).ToArray();
        }

        for (int i = 0; i < children.Length; i++)
        {
            var isChildLast = i == children.Length - 1;
            if (children[i].AsNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, newIndent, false, isChildLast, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth, currentDepth + 1);
            }
            else if (includeTokens && children[i].AsToken(out var token))
            {
                // Include trivia if specified
                if (includeTrivia)
                {
                    var triviaList = token.LeadingTrivia;
                    PrintTrivia(triviaList, true, sb, newIndent, false, isChildLast, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth, currentDepth + 1);
                }

                // Print token
                sb.AppendLine($"{newIndent}{(isChildLast ? MarkerBottom : MarkerMiddle)}" + Colorize($"{token.Kind}", ConsoleColor.Green) + $":{(token.IsMissing ? " (Missing)" : "")} \"{token.Text}\"{(includeSpans ? $" {Span(token.Span)}" : string.Empty)}{(includeLocation ? $" {Location(token.GetLocation())}" : string.Empty)}");

                // Include trivia if specified
                if (includeTrivia)
                {
                    var triviaList = token.TrailingTrivia;
                    PrintTrivia(triviaList, false, sb, newIndent, false, isChildLast, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth, currentDepth + 1);
                }
            }
        }
    }

    private static void PrintTrivia(SyntaxTriviaList triviaList, bool isLeading, StringBuilder sb, string indent, bool isFirst, bool isLast, bool includeTokens, bool includeTrivia, bool includeSpans, bool includeLocation, int maxDepth, int currentDepth = 0)
    {
        string marker = string.Empty;

        if (isLeading)
        {
            marker = MarkerTop;
        }
        else
        {
            marker = MarkerBottom;
        }

        var newIndent = isFirst ? string.Empty : indent + (isLast ? IndentationStr : MarkerStraight);

        var listCount = triviaList.Count;
        for (int i = 0; i < listCount; i++)
        {
            var trivia = triviaList[i];
            var isFirstChild = i == 0;
            bool isChildLast = i == (listCount - 1);

            var firstMarker = isLeading ? MarkerTop : MarkerBottom;

            sb.AppendLine($"{newIndent}{(isChildLast ? firstMarker : MarkerMiddle)}" + Colorize($"{trivia.Kind}", ConsoleColor.Red) + $": \"{TriviaToString(trivia)}\"{(includeSpans ? $" {Span(trivia.Span)}" : string.Empty)}{(includeLocation ? $" {Location(trivia.GetLocation())}" : string.Empty)}");

            var newIndent2 = isFirstChild ? string.Empty : newIndent + (isChildLast ? IndentationStr : MarkerStraight);

            if (trivia.HasStructure)
            {
                PrintStructuredTrivia(sb, includeSpans, includeLocation, trivia, isFirstChild, isChildLast, newIndent2);
            }
        }
    }

    private static void PrintStructuredTrivia(StringBuilder sb, bool includeSpans, bool includeLocation, SyntaxTrivia trivia, bool isFirstChild, bool isChildLast, string newIndent2)
    {
        var structure = trivia.GetStructure()!;
        sb.AppendLine($"{newIndent2}{(MarkerBottom)}" + Colorize($"{structure.Kind}", ConsoleColor.Red) + $": \"{structure.ToString()}\"{(includeSpans ? $" {Span(structure.Span)}" : string.Empty)}{(includeLocation ? $" {Location(structure.GetLocation())}" : string.Empty)}");

        int i2 = 0;
        var structureChildren = structure.ChildNodesAndTokens();
        var structureCount = structureChildren.Count();
        foreach (var triviaChild in structureChildren)
        {
            var isChildLast2 = i2 == structureCount - 1;

            var newIndent4 = isFirstChild ? string.Empty : newIndent2 + (isChildLast2 ? IndentationStr : MarkerStraight);

            if (triviaChild.AsToken(out var token))
            {
                sb.AppendLine($"{newIndent4}{(isChildLast2 ? MarkerBottom : MarkerMiddle)}" + Colorize($"{token.Kind}", ConsoleColor.Green) + $": \"{token.ToString()}\"{(includeSpans ? $" {Span(token.Span)}" : string.Empty)}{(includeLocation ? $" {Location(token.GetLocation())}" : string.Empty)}");
            }
            i2++;
        }
    }

    private static string Location(Location location)
    {
        var lineSpan = location.GetLineSpan();
        var start = lineSpan.StartLinePosition;
        return $"({(start.Line + 1)}:{start.Character + 1})";
    }

    private static string Span(TextSpan span)
    {
        return $"[{span}]";
    }

    private static string TriviaToString(SyntaxTrivia trivia)
    {
        return trivia.ToString().Replace("\r", @"\r")
            .Replace("\n", @"\n")
            .Replace("\t", @"\t");
    }

    private static string Colorize(string text, ConsoleColor color)
    {
        return $"\u001b[{GetColorCode(color)}m{text}\u001b[0m";
    }

    private static int GetColorCode(ConsoleColor color)
    {
        return color switch
        {
            ConsoleColor.Blue => 34,
            ConsoleColor.Green => 32,
            ConsoleColor.Yellow => 33,
            ConsoleColor.Red => 31,
            ConsoleColor.Cyan => 36,
            _ => 37
        };
    }
}
