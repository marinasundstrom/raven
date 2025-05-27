using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;




public class ColorScheme
{
    public AnsiColor Default { get; internal set; }

    public AnsiColor Method { get; internal set; }

    public AnsiColor Namespace { get; internal set; }

    public AnsiColor Type { get; internal set; }

    public AnsiColor Keyword { get; internal set; }

    public AnsiColor StringLiteral { get; internal set; }

    public AnsiColor Comment { get; internal set; }

    public static ColorScheme Light { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightBlack,
        Method = AnsiColor.BrightRed,
        Namespace = AnsiColor.BrightCyan,
        Type = AnsiColor.Magenta,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightYellow,
        Comment = AnsiColor.Green
    };

    public static ColorScheme Dark { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightWhite,
        Method = AnsiColor.BrightYellow,
        Namespace = AnsiColor.BrightCyan,
        Type = AnsiColor.BrightMagenta,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightRed,
        Comment = AnsiColor.BrightGreen
    };
}

public static class ConsoleSyntaxHighlighter
{
    public static ColorScheme ColorScheme { get; set; } = ColorScheme.Dark;

    public static Compilation Compilation { get; private set; }
    public static SemanticModel SemanticModel { get; private set; }

    private static Dictionary<SyntaxToken, SemanticClassification> _classificationMap;

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation)
    {
        Compilation = compilation;
        SemanticModel = compilation.GetSemanticModel(node.SyntaxTree);

        _classificationMap = SemanticClassifier.Classify(node, SemanticModel);

        var sb = new StringBuilder();
        WriteNode(node, sb);
        return sb.ToString();
    }

    private static void WriteNode(SyntaxNode node, StringBuilder sb)
    {
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.IsToken)
            {
                WriteToken(child.AsToken(), sb);
            }
            else
            {
                WriteNode(child.AsNode()!, sb);
            }
        }
    }

    private static void WriteToken(SyntaxToken token, StringBuilder sb)
    {
        WriteTriviaList(token.LeadingTrivia, sb);

        var color = GetColorForToken(token);
        AppendAnsiColor(sb, color);
        sb.Append(token.Text);
        AppendAnsiColor(sb, AnsiColor.Reset);

        WriteTriviaList(token.TrailingTrivia, sb);
    }

    private static void WriteTriviaList(SyntaxTriviaList triviaList, StringBuilder sb)
    {
        foreach (var trivia in triviaList)
        {
            if (trivia.HasStructure)
            {
                WriteStructuredTrivia(trivia.GetStructure()!, sb);
            }
            else
            {
                var color = GetColorForTrivia(trivia);
                AppendAnsiColor(sb, color);
                sb.Append(trivia.Text);
                AppendAnsiColor(sb, AnsiColor.Reset);
            }
        }
    }

    private static void WriteStructuredTrivia(SyntaxNode structure, StringBuilder sb)
    {
        // Structured trivia are mini trees (e.g. documentation comments, directives)
        foreach (var child in structure.ChildNodesAndTokens())
        {
            if (child.IsToken)
            {
                WriteToken(child.AsToken(), sb);
            }
            else
            {
                WriteStructuredTrivia(child.AsNode()!, sb);
            }
        }
    }

    private static AnsiColor GetColorForTrivia(SyntaxTrivia trivia)
    {
        return trivia.Kind switch
        {
            SyntaxKind.SingleLineCommentTrivia => ColorScheme.Comment,
            SyntaxKind.MultiLineCommentTrivia => ColorScheme.Comment,
            _ => ColorScheme.Default
        };
    }

    private static AnsiColor GetColorForToken(SyntaxToken token)
    {
        if (!_classificationMap.TryGetValue(token, out var classification))
            return ColorScheme.Default;

        return classification switch
        {
            SemanticClassification.Keyword => ColorScheme.Keyword,
            SemanticClassification.StringLiteral => ColorScheme.StringLiteral,
            SemanticClassification.Comment => ColorScheme.Comment,
            SemanticClassification.Method => ColorScheme.Method,
            SemanticClassification.Type => ColorScheme.Type,
            SemanticClassification.Namespace => ColorScheme.Namespace,
            _ => ColorScheme.Default
        };
    }

    private static void AppendAnsiColor(StringBuilder sb, AnsiColor color)
    {
        sb.Append($"\u001b[{(int)color}m");
    }
}