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
    public AnsiColor NumericLiteral { get; internal set; }
    public AnsiColor Comment { get; internal set; }
    public AnsiColor Field { get; internal set; }
    public AnsiColor Parameter { get; internal set; }

    public static ColorScheme Light { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightBlack,
        Method = AnsiColor.BrightRed,
        Namespace = AnsiColor.BrightCyan,
        Type = AnsiColor.Magenta,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightYellow,
        NumericLiteral = AnsiColor.Yellow,
        Comment = AnsiColor.Green,
        Field = AnsiColor.Cyan,
        Parameter = AnsiColor.Blue
    };

    public static ColorScheme Dark { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightWhite,
        Method = AnsiColor.BrightYellow,
        Namespace = AnsiColor.BrightCyan,
        Type = AnsiColor.BrightMagenta,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightRed,
        NumericLiteral = AnsiColor.Red,
        Comment = AnsiColor.BrightGreen,
        Field = AnsiColor.Cyan,
        Parameter = AnsiColor.Blue
    };
}

public static class ConsoleSyntaxHighlighter
{
    public static ColorScheme ColorScheme { get; set; } = ColorScheme.Dark;

    public static Compilation Compilation { get; private set; }
    public static SemanticModel SemanticModel { get; private set; }

    private static Dictionary<SyntaxToken, SemanticClassification> _classificationMap;
    private static Dictionary<SyntaxTrivia, SemanticClassification> _triviaClassificationMap;

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation)
    {
        Compilation = compilation;
        SemanticModel = compilation.GetSemanticModel(node.SyntaxTree);

        var result = SemanticClassifier.Classify(node, SemanticModel);
        _classificationMap = result.Tokens;
        _triviaClassificationMap = result.Trivia;

        var sb = new StringBuilder();
        WriteNode(node, sb);
        return sb.ToString();
    }

    private static void WriteNode(SyntaxNode node, StringBuilder sb)
    {
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.IsToken)
                WriteToken(child.AsToken(), sb);
            else
                WriteNode(child.AsNode()!, sb);
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
        foreach (var child in structure.ChildNodesAndTokens())
        {
            if (child.IsToken)
                WriteToken(child.AsToken(), sb);
            else
                WriteStructuredTrivia(child.AsNode()!, sb);
        }
    }

    private static AnsiColor GetColorForToken(SyntaxToken token)
    {
        if (!_classificationMap.TryGetValue(token, out var classification))
            return ColorScheme.Default;

        return classification switch
        {
            SemanticClassification.Keyword => ColorScheme.Keyword,
            SemanticClassification.StringLiteral => ColorScheme.StringLiteral,
            SemanticClassification.NumericLiteral => ColorScheme.NumericLiteral,
            SemanticClassification.Comment => ColorScheme.Comment,
            SemanticClassification.Method => ColorScheme.Method,
            SemanticClassification.Type => ColorScheme.Type,
            SemanticClassification.Namespace => ColorScheme.Namespace,
            SemanticClassification.Field => ColorScheme.Field,
            SemanticClassification.Parameter => ColorScheme.Parameter,
            _ => ColorScheme.Default
        };
    }

    private static AnsiColor GetColorForTrivia(SyntaxTrivia trivia)
    {
        if (_triviaClassificationMap.TryGetValue(trivia, out var classification))
        {
            return classification switch
            {
                SemanticClassification.Comment => ColorScheme.Comment,
                _ => ColorScheme.Default
            };
        }

        return ColorScheme.Default;
    }

    private static void AppendAnsiColor(StringBuilder sb, AnsiColor color)
    {
        sb.Append($"\u001b[{(int)color}m");
    }
}