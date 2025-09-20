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
    private record struct TokenSpan(int Start, int End, SemanticClassification Classification);
    private record struct DiagnosticSpan(int Start, int End, DiagnosticSeverity Severity);

    private static readonly bool s_supportsAnsi = true;

    public static ColorScheme ColorScheme { get; set; } = ColorScheme.Dark;

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation, bool includeDiagnostics = false)
    {
        if (!s_supportsAnsi)
            return node.SyntaxTree.GetText()!.ToString();

        var model = compilation.GetSemanticModel(node.SyntaxTree);
        var classification = SemanticClassifier.Classify(node, model);

        var sourceText = node.SyntaxTree.GetText()!;
        var text = sourceText.ToString();
        var lines = text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');

        var lineTokens = new List<TokenSpan>[lines.Length];
        var lineDiagnostics = new List<DiagnosticSpan>[lines.Length];

        foreach (var kvp in classification.Tokens)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTokenSpan(lineTokens, lines, sourceText, kvp.Key.Span, kvp.Value);
        }

        foreach (var kvp in classification.Trivia)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTokenSpan(lineTokens, lines, sourceText, kvp.Key.Span, kvp.Value);
        }

        if (includeDiagnostics)
        {
            foreach (var diagnostic in compilation.GetDiagnostics()
                         .Where(d => d.Location.SourceTree == node.SyntaxTree))
            {
                AddDiagnosticSpan(lineDiagnostics, lines, sourceText, diagnostic.Location.SourceSpan.Start,
                    diagnostic.Location.SourceSpan.End, diagnostic.Severity);
            }
        }

        var sb = new StringBuilder();
        for (int i = 0; i < lines.Length; i++)
        {
            AppendLine(sb, lines[i], lineTokens[i], lineDiagnostics[i]);
            if (i < lines.Length - 1)
                sb.AppendLine();
        }

        return sb.ToString();
    }

    private static void AppendLine(StringBuilder sb, string line, List<TokenSpan>? tokens, List<DiagnosticSpan>? diagnostics)
    {
        tokens ??= new();
        diagnostics ??= new();

        var currentColor = AnsiColor.Reset;
        DiagnosticSeverity? currentSeverity = null;

        for (var idx = 0; idx < line.Length; idx++)
        {
            var severity = GetSeverityAt(diagnostics, idx);
            if (severity != currentSeverity)
            {
                if (currentSeverity.HasValue)
                    sb.Append(UnderlineEnd);
                if (severity.HasValue)
                    sb.Append(GetUnderlineStart(severity.Value));
                currentSeverity = severity;
            }

            var classification = GetClassificationAt(tokens, idx);
            var color = GetColorForClassification(classification);
            if (color != currentColor)
            {
                sb.Append(GetAnsiColor(color));
                currentColor = color;
            }

            sb.Append(line[idx]);
        }

        if (currentColor != AnsiColor.Reset)
            sb.Append(GetAnsiColor(AnsiColor.Reset));
        if (currentSeverity.HasValue)
            sb.Append(UnderlineEnd);
    }

    private static DiagnosticSeverity? GetSeverityAt(List<DiagnosticSpan> diagnostics, int index)
    {
        foreach (var span in diagnostics)
        {
            if (index >= span.Start && index < span.End)
                return span.Severity;
        }
        return null;
    }

    private static SemanticClassification GetClassificationAt(List<TokenSpan> tokens, int index)
    {
        foreach (var span in tokens)
        {
            if (index >= span.Start && index < span.End)
                return span.Classification;
        }
        return SemanticClassification.Default;
    }

    private static void AddTokenSpan(List<TokenSpan>[] lineTokens, string[] lines, SourceText text, TextSpan span,
        SemanticClassification classification)
    {
        var (startLine1, startCol1) = text.GetLineAndColumn(span);
        var (endLine1, endCol1) = text.GetLineAndColumn(new TextSpan(span.End, 0));
        var startLine = startLine1 - 1;
        var startCol = startCol1 - 1;
        var endLine = endLine1 - 1;
        var endCol = endCol1 - 1;
        for (var line = startLine; line <= endLine; line++)
        {
            var start = line == startLine ? startCol : 0;
            var end = line == endLine ? endCol : lines[line].Length;
            var list = lineTokens[line] ??= new List<TokenSpan>();
            list.Add(new TokenSpan(start, end, classification));
        }
    }

    private static void AddDiagnosticSpan(List<DiagnosticSpan>[] lineDiagnostics, string[] lines, SourceText text,
        int startPos, int endPos, DiagnosticSeverity severity)
    {
        var (startLine1, startCol1) = text.GetLineAndColumn(new TextSpan(startPos, 0));
        var (endLine1, endCol1) = text.GetLineAndColumn(new TextSpan(endPos, 0));
        var startLine = startLine1 - 1;
        var startCol = startCol1 - 1;
        var endLine = endLine1 - 1;
        var endCol = endCol1 - 1;
        for (var line = startLine; line <= endLine; line++)
        {
            var start = line == startLine ? startCol : 0;
            var end = line == endLine ? endCol : lines[line].Length;
            var list = lineDiagnostics[line] ??= new List<DiagnosticSpan>();
            list.Add(new DiagnosticSpan(start, end, severity));
        }
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
