using System.Collections.Generic;
using System.Linq;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

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
    public AnsiColor Property { get; internal set; }
    public AnsiColor Local { get; internal set; }
    public AnsiColor Error { get; internal set; }
    public AnsiColor Warning { get; internal set; }
    public AnsiColor Info { get; internal set; }

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
        Parameter = AnsiColor.Blue,
        Property = AnsiColor.BrightGreen,
        Local = AnsiColor.BrightMagenta,
        Error = AnsiColor.BrightRed,
        Warning = AnsiColor.BrightGreen,
        Info = AnsiColor.BrightBlue
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
        Parameter = AnsiColor.Blue,
        Property = AnsiColor.Green,
        Local = AnsiColor.Magenta,
        Error = AnsiColor.BrightRed,
        Warning = AnsiColor.BrightGreen,
        Info = AnsiColor.BrightBlue
    };
}

public static class ConsoleSyntaxHighlighter
{
    private record struct DiagnosticSpan(int Start, int End, DiagnosticSeverity Severity);

    private static readonly bool s_supportsAnsi = true;

    public static ColorScheme ColorScheme { get; set; } = ColorScheme.Dark;

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation, bool includeDiagnostics = false)
    {
        if (!s_supportsAnsi)
            return node.SyntaxTree!.GetText()!.ToString();

        var syntaxTree = node.SyntaxTree!;
        var model = compilation.GetSemanticModel(syntaxTree);
        var classification = SemanticClassifier.Classify(node, model);

        DiagnosticSpan[] diagnosticSpans = Array.Empty<DiagnosticSpan>();
        if (includeDiagnostics)
        {
            diagnosticSpans = compilation.GetDiagnostics()
                .Where(d => d.Location.SourceTree == syntaxTree)
                .Select(d => new DiagnosticSpan(d.Location.SourceSpan.Start, d.Location.SourceSpan.End, d.Severity))
                .OrderBy(span => span.Start)
                .ToArray();
        }

        var sb = new StringBuilder();
        var currentColor = AnsiColor.Reset;
        DiagnosticSeverity? currentSeverity = null;
        var position = node.FullSpan.Start;
        var diagnosticIndex = 0;

        void AppendSegment(string segment, SemanticClassification classificationKind)
        {
            if (segment.Length == 0)
                return;

            var targetColor = GetColorForClassification(classificationKind);

            foreach (var ch in segment)
            {
                DiagnosticSeverity? severity = null;
                if (includeDiagnostics)
                {
                    while (diagnosticIndex < diagnosticSpans.Length && diagnosticSpans[diagnosticIndex].End <= position)
                        diagnosticIndex++;

                    if (diagnosticIndex < diagnosticSpans.Length)
                    {
                        var span = diagnosticSpans[diagnosticIndex];
                        if (position >= span.Start && position < span.End)
                            severity = span.Severity;
                    }
                }

                if (severity != currentSeverity)
                {
                    if (currentSeverity.HasValue)
                        sb.Append(UnderlineEnd);
                    if (severity.HasValue)
                        sb.Append(GetUnderlineStart(severity.Value));
                    currentSeverity = severity;
                }

                if (targetColor != currentColor)
                {
                    sb.Append(GetAnsiColor(targetColor));
                    currentColor = targetColor;
                }

                sb.Append(ch);
                position++;
            }
        }

        foreach (var token in node.DescendantTokens(descendIntoTrivia: true))
        {
            foreach (var trivia in token.LeadingTrivia)
            {
                classification.Trivia.TryGetValue(trivia, out var triviaClassification);
                AppendSegment(trivia.ToString(), triviaClassification);
            }

            classification.Tokens.TryGetValue(token, out var tokenClassification);
            AppendSegment(token.ToString(), tokenClassification);

            foreach (var trivia in token.TrailingTrivia)
            {
                classification.Trivia.TryGetValue(trivia, out var triviaClassification);
                AppendSegment(trivia.ToString(), triviaClassification);
            }
        }

        if (currentColor != AnsiColor.Reset)
            sb.Append(GetAnsiColor(AnsiColor.Reset));
        if (currentSeverity.HasValue)
            sb.Append(UnderlineEnd);

        return sb.ToString();
    }

    private static AnsiColor GetColorForClassification(SemanticClassification classification) => classification switch
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
        SemanticClassification.Property => ColorScheme.Property,
        SemanticClassification.Local => ColorScheme.Local,
        _ => ColorScheme.Default
    };

    private static string GetUnderlineStart(DiagnosticSeverity severity)
    {
        var color = GetColorForSeverity(severity);
        var underlineColor = color switch
        {
            AnsiColor.BrightRed => 9,
            AnsiColor.BrightGreen => 10,
            AnsiColor.BrightBlue => 12,
            _ => 12
        };
        return $"\u001b[4:3m\u001b[58;5;{underlineColor}m";
    }

    private static string UnderlineEnd => "\u001b[4:0m\u001b[59m";

    private static AnsiColor GetColorForSeverity(DiagnosticSeverity severity) => severity switch
    {
        DiagnosticSeverity.Error => ColorScheme.Error,
        DiagnosticSeverity.Warning => ColorScheme.Warning,
        DiagnosticSeverity.Info => ColorScheme.Info,
        _ => ColorScheme.Info
    };

    private static string GetAnsiColor(AnsiColor color) => $"\u001b[{(int)color}m";
}
