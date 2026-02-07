using System;
using System.Collections.Generic;
using System.IO;
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
    public AnsiColor Interpolation { get; internal set; }
    public AnsiColor NumericLiteral { get; internal set; }
    public AnsiColor Comment { get; internal set; }
    public AnsiColor Field { get; internal set; }
    public AnsiColor Parameter { get; internal set; }
    public AnsiColor Property { get; internal set; }
    public AnsiColor Local { get; internal set; }
    public AnsiColor Label { get; internal set; }
    public AnsiColor Event { get; internal set; }
    public AnsiColor NullableAnnotation { get; internal set; }
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
        Interpolation = AnsiColor.Cyan,
        NumericLiteral = AnsiColor.Yellow,
        Comment = AnsiColor.Green,
        Field = AnsiColor.Cyan,
        Parameter = AnsiColor.Blue,
        Property = AnsiColor.BrightGreen,
        Local = AnsiColor.BrightMagenta,
        Label = AnsiColor.White,
        Event = AnsiColor.BrightCyan,
        NullableAnnotation = AnsiColor.BrightBlack,
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
        Interpolation = AnsiColor.BrightCyan,
        NumericLiteral = AnsiColor.Red,
        Comment = AnsiColor.BrightGreen,
        Field = AnsiColor.Cyan,
        Parameter = AnsiColor.Blue,
        Property = AnsiColor.Green,
        Local = AnsiColor.Magenta,
        Label = AnsiColor.BrightWhite,
        Event = AnsiColor.BrightCyan,
        NullableAnnotation = AnsiColor.BrightBlack,
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

    public static string WriteTextToTextLight(string text, ParseOptions? options = null)
    {
        if (!s_supportsAnsi)
            return text;

        var syntaxTree = SyntaxTree.ParseText(text, options);
        var root = syntaxTree.GetRoot();
        var sourceText = syntaxTree.GetText()!;
        var source = sourceText.ToString();
        var lines = source.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');
        var lineTokens = new List<TokenSpan>[lines.Length];

        foreach (var token in root.DescendantTokens())
        {
            var tokenClassification = GetTokenClassificationForToken(token);
            if (tokenClassification != SemanticClassification.Default)
                AddTokenSpan(lineTokens, lines, sourceText, token, tokenClassification);

            foreach (var trivia in token.LeadingTrivia)
            {
                var triviaClassification = GetTriviaClassificationForKind(trivia.Kind);
                if (triviaClassification != SemanticClassification.Default)
                    AddTriviaSpan(lineTokens, lines, sourceText, trivia.Span, triviaClassification);
            }

            foreach (var trivia in token.TrailingTrivia)
            {
                var triviaClassification = GetTriviaClassificationForKind(trivia.Kind);
                if (triviaClassification != SemanticClassification.Default)
                    AddTriviaSpan(lineTokens, lines, sourceText, trivia.Span, triviaClassification);
            }
        }

        var sb = new StringBuilder();
        for (var index = 0; index < lines.Length; index++)
        {
            AppendLine(sb, lines[index], lineTokens[index], diagnostics: null);
            if (index < lines.Length - 1)
                sb.AppendLine();
        }

        return sb.ToString();
    }

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation, bool includeDiagnostics = false,
        bool diagnosticsOnly = false, IEnumerable<Diagnostic>? diagnostics = null, bool includeSuggestions = false)
    {
        var syntaxTree = node.SyntaxTree ?? throw new InvalidOperationException("Node is not associated with a syntax tree.");

        if (!s_supportsAnsi)
            return syntaxTree.GetText()!.ToString();

        if (diagnosticsOnly)
            includeDiagnostics = true;

        var model = compilation.GetSemanticModel(syntaxTree);
        var classification = SemanticClassifier.Classify(node, model);

        var sourceText = syntaxTree.GetText()!;
        var text = sourceText.ToString();
        var lines = text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');

        var lineTokens = new List<TokenSpan>[lines.Length];
        var lineDiagnostics = new List<DiagnosticSpan>[lines.Length];

        foreach (var kvp in classification.Tokens)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTokenSpan(lineTokens, lines, sourceText, kvp.Key, kvp.Value);
        }

        foreach (var kvp in classification.Trivia)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTriviaSpan(lineTokens, lines, sourceText, kvp.Key.Span, kvp.Value);
        }

        IReadOnlyList<Diagnostic> diagnosticsForTree = Array.Empty<Diagnostic>();
        if (includeDiagnostics)
        {
            var sourceDiagnostics = diagnostics ?? compilation.GetDiagnostics();

            diagnosticsForTree = sourceDiagnostics
                .Where(d => d.Location.SourceTree == syntaxTree)
                .OrderBy(d => d.Location.SourceSpan.Start)
                .ThenBy(d => d.Location.SourceSpan.Length)
                .ToArray();

            foreach (var diagnostic in diagnosticsForTree)
            {
                AddDiagnosticSpan(lineDiagnostics, lines, sourceText, diagnostic.Location.SourceSpan.Start,
                    diagnostic.Location.SourceSpan.End, diagnostic.Severity);
            }
        }

        var sb = new StringBuilder();
        if (diagnosticsOnly)
            return WriteDiagnosticsOnly(sb, diagnosticsForTree, lines, lineTokens, syntaxTree.FilePath, includeSuggestions);

        var lineOrder = Enumerable.Range(0, lines.Length).ToArray();

        for (var index = 0; index < lineOrder.Length; index++)
        {
            var lineIndex = lineOrder[index];
            AppendLine(sb, lines[lineIndex], lineTokens[lineIndex], lineDiagnostics[lineIndex]);
            if (index < lineOrder.Length - 1)
                sb.AppendLine();
        }

        return sb.ToString();
    }

    private static string WriteDiagnosticsOnly(StringBuilder sb, IReadOnlyList<Diagnostic> diagnostics, string[] lines,
        IReadOnlyList<List<TokenSpan>?> lineTokens, string? fallbackPath,
        bool includeSuggestions)
    {
        if (diagnostics.Count == 0)
            return string.Empty;

        for (var i = 0; i < diagnostics.Count; i++)
        {
            var diagnostic = diagnostics[i];
            var span = diagnostic.Location.GetLineSpan();
            var filePath = span.Path;

            if (string.IsNullOrEmpty(filePath))
                filePath = fallbackPath;

            string? fileDirectory = null;
            string? fileName = null;

            if (!string.IsNullOrEmpty(filePath))
            {
                try
                {
                    var relative = Path.GetRelativePath(Environment.CurrentDirectory, filePath);
                    if (!string.IsNullOrEmpty(relative) && relative != ".")
                        filePath = relative;
                }
                catch (Exception)
                {
                    // Ignore failures computing relative paths and fall back to the original path.
                }

                filePath = filePath.Replace('\\', '/');

                fileDirectory = Path.GetDirectoryName(filePath);
                if (!string.IsNullOrEmpty(fileDirectory))
                    fileDirectory = fileDirectory.Replace('\\', '/') + "/";

                fileName = Path.GetFileName(filePath);
            }

            fileDirectory ??= string.Empty;
            fileName ??= filePath ?? string.Empty;

            var start = span.StartLinePosition;
            var severityColor = GetColorForSeverity(diagnostic.Severity);
            var severityAnsi = GetAnsiColor(severityColor);
            var resetAnsi = GetAnsiColor(AnsiColor.Reset);
            var severityText = diagnostic.Severity.ToString().ToLowerInvariant();

            if (fileDirectory.Length > 0)
                sb.Append(fileDirectory);

            sb.Append(BoldStart);
            sb.Append(fileName);
            sb.Append(BoldEnd);
            sb.Append('(');
            sb.Append(start.Line + 1);
            sb.Append(',');
            sb.Append(start.Character + 1);
            sb.Append("):");
            sb.Append(' ');
            sb.Append(severityAnsi);
            sb.Append(BoldStart);
            sb.Append(severityText);
            sb.Append(' ');
            sb.Append(diagnostic.Descriptor.Id);
            sb.Append(BoldEnd);
            sb.Append(resetAnsi);
            sb.Append(':');
            sb.Append(' ');
            sb.AppendLine(diagnostic.GetMessage());

            sb.AppendLine();

            var end = span.EndLinePosition;
            var startLine = Math.Max(start.Line, 0);
            var endLine = Math.Max(end.Line, startLine);

            for (var lineIndex = startLine; lineIndex <= endLine && lineIndex < lines.Length; lineIndex++)
            {
                var currentDiagnosticLineSpans = GetDiagnosticSpansForLine(
                    start,
                    end,
                    lineIndex,
                    lines[lineIndex].Length,
                    diagnostic.Severity);

                sb.Append("    ");
                AppendLine(sb, lines[lineIndex], lineTokens[lineIndex], currentDiagnosticLineSpans);
                sb.AppendLine();
            }

            if (includeSuggestions &&
                EducationalDiagnosticProperties.TryGetRewriteSuggestion(diagnostic, out var originalCode, out var rewrittenCode))
            {
                var highlightedOriginal = WriteTextToTextLight(originalCode);
                var highlightedSuggestion = WriteTextToTextLight(rewrittenCode);

                sb.AppendLine();
                sb.AppendLine("  You wrote:");
                foreach (var line in highlightedOriginal.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n'))
                {
                    sb.Append("    ");
                    sb.AppendLine(line);
                }

                sb.AppendLine();
                sb.AppendLine("  Write this instead:");
                foreach (var line in highlightedSuggestion.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n'))
                {
                    sb.Append("    ");
                    sb.AppendLine(line);
                }
            }

            if (i < diagnostics.Count - 1)
                sb.AppendLine();
        }

        return sb.ToString();
    }

    private static List<DiagnosticSpan>? GetDiagnosticSpansForLine(
        LinePosition start,
        LinePosition end,
        int lineIndex,
        int lineLength,
        DiagnosticSeverity severity)
    {
        if (lineIndex < start.Line || lineIndex > end.Line)
            return null;

        var spanStart = lineIndex == start.Line ? start.Character : 0;
        var spanEnd = lineIndex == end.Line ? end.Character : lineLength;

        spanStart = Math.Clamp(spanStart, 0, lineLength);
        spanEnd = Math.Clamp(spanEnd, 0, lineLength);

        if (spanEnd <= spanStart)
            return null;

        return [new DiagnosticSpan(spanStart, spanEnd, severity)];
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
        SemanticClassification best = SemanticClassification.Default;
        var bestPri = -1;

        foreach (var span in tokens)
        {
            if (index < span.Start || index >= span.End)
                continue;

            var pri = GetClassificationPriority(span.Classification);
            if (pri > bestPri)
            {
                bestPri = pri;
                best = span.Classification;
            }
        }

        return best;
    }

    private static int GetClassificationPriority(SemanticClassification c) => c switch
    {
        // trivia-ish / strongest overlays
        SemanticClassification.Comment => 100,

        // literals
        SemanticClassification.StringLiteral => 90,
        SemanticClassification.Interpolation => 85,
        SemanticClassification.NumericLiteral => 80,

        // language / semantic
        SemanticClassification.Keyword => 70,
        SemanticClassification.Type => 65,
        SemanticClassification.Namespace => 60,
        SemanticClassification.Method => 55,

        // symbols
        SemanticClassification.Property => 50,
        SemanticClassification.Event => 47,
        SemanticClassification.Field => 45,
        SemanticClassification.Parameter => 40,
        SemanticClassification.Local => 35,
        SemanticClassification.Label => 30,
        SemanticClassification.NullableAnnotation => 25,

        _ => 0
    };

    private static void AddTokenSpan(List<TokenSpan>[] lineTokens, string[] lines, SourceText text, SyntaxToken token,
        SemanticClassification classification)
    {
        AddTokenSpan(lineTokens, lines, text, token.Span, classification, token.Text);
    }

    private static void AddTriviaSpan(List<TokenSpan>[] lineTokens, string[] lines, SourceText text, TextSpan span,
        SemanticClassification classification)
    {
        AddTokenSpan(lineTokens, lines, text, span, classification, tokenText: null);
    }

    private static void AddTokenSpan(
    List<TokenSpan>[] lineTokens,
    string[] lines,
    SourceText text,
    TextSpan span,
    SemanticClassification classification,
    string? tokenText)
    {
        var (startLine1, startCol1) = text.GetLineAndColumn(span);
        var (endLine1, endCol1) = text.GetLineAndColumn(new TextSpan(span.End, 0));

        var startLine = startLine1 - 1;
        var startCol = startCol1 - 1;
        var endLine = endLine1 - 1;
        var endCol = endCol1 - 1;

        for (var line = startLine; line <= endLine; line++)
        {
            var lineText = lines[line];

            var start = line == startLine ? startCol : 0;
            var end = line == endLine ? endCol : lineText.Length;

            start = Math.Clamp(start, 0, lineText.Length);
            end = Math.Clamp(end, 0, lineText.Length);

            if (end <= start)
                continue;

            // IMPORTANT: only attempt “relocation” if the expected slice doesn't match.
            // This prevents “Foo() -> Foo” from highlighting the wrong Foo.
            if (tokenText is { Length: > 0 } && startLine == endLine)
            {
                var expectedLen = Math.Min(tokenText.Length, lineText.Length - start);
                var slice = expectedLen > 0 ? lineText.Substring(start, expectedLen) : string.Empty;

                if (!slice.Equals(tokenText, StringComparison.Ordinal))
                {
                    // Search in a small window near the expected location and pick the closest match.
                    var windowStart = Math.Max(0, start - 8);
                    var windowEnd = Math.Min(lineText.Length, end + 8);
                    var windowLen = windowEnd - windowStart;

                    if (windowLen > 0)
                    {
                        var window = lineText.Substring(windowStart, windowLen);

                        var bestIndex = -1;
                        var bestDistance = int.MaxValue;

                        for (var idx = window.IndexOf(tokenText, StringComparison.Ordinal);
                             idx >= 0;
                             idx = window.IndexOf(tokenText, idx + 1, StringComparison.Ordinal))
                        {
                            var absolute = windowStart + idx;
                            var dist = Math.Abs(absolute - start);
                            if (dist < bestDistance)
                            {
                                bestDistance = dist;
                                bestIndex = absolute;
                            }
                        }

                        if (bestIndex >= 0)
                        {
                            start = bestIndex;
                            end = Math.Min(lineText.Length, bestIndex + tokenText.Length);
                        }
                    }
                }
            }

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

    private static SemanticClassification GetTokenClassificationForToken(SyntaxToken token)
    {
        var kind = token.Kind;
        if (SyntaxFacts.IsKeywordKind(kind))
            return SemanticClassification.Keyword;

        if (kind == SyntaxKind.IdentifierToken)
        {
            if (IsLabelIdentifier(token))
                return SemanticClassification.Label;
            if (IsNamespaceIdentifier(token))
                return SemanticClassification.Namespace;
            if (IsTypeIdentifier(token))
                return SemanticClassification.Type;
            if (IsMethodIdentifier(token))
                return SemanticClassification.Method;
            if (IsPropertyIdentifier(token))
                return SemanticClassification.Property;
            if (IsEventIdentifier(token))
                return SemanticClassification.Event;
            if (IsFieldIdentifier(token))
                return SemanticClassification.Field;
            if (IsParameterIdentifier(token))
                return SemanticClassification.Parameter;
            if (IsLocalIdentifier(token))
                return SemanticClassification.Local;
        }

        return kind switch
        {
            SyntaxKind.NumericLiteralToken => SemanticClassification.NumericLiteral,
            SyntaxKind.StringLiteralToken => SemanticClassification.StringLiteral,
            SyntaxKind.MultiLineStringLiteralToken => SemanticClassification.StringLiteral,
            SyntaxKind.MultilineStringToken => SemanticClassification.StringLiteral,
            SyntaxKind.CharacterLiteralToken => SemanticClassification.StringLiteral,
            SyntaxKind.StringStartToken => SemanticClassification.StringLiteral,
            SyntaxKind.StringEndToken => SemanticClassification.StringLiteral,
            SyntaxKind.DollarToken => SemanticClassification.Interpolation,
            _ => SemanticClassification.Default
        };
    }

    private static bool IsLabelIdentifier(SyntaxToken token)
        => token.Parent?.Kind == SyntaxKind.LabeledStatement;

    private static bool IsNamespaceIdentifier(SyntaxToken token)
        => HasAncestorKind(token.Parent, SyntaxKind.NamespaceDeclaration)
            || HasAncestorKind(token.Parent, SyntaxKind.FileScopedNamespaceDeclaration)
            || HasAncestorKind(token.Parent, SyntaxKind.ImportDirective)
            || HasAncestorKind(token.Parent, SyntaxKind.AliasDirective)
            || HasAncestorKind(token.Parent, SyntaxKind.AliasQualifiedName);

    private static bool IsTypeIdentifier(SyntaxToken token)
    {
        if (token.Parent is null)
            return false;

        if (token.Parent.Kind == SyntaxKind.MemberPatternPath)
            return true;

        if (HasAncestorKind(token.Parent, SyntaxKind.TypeAnnotationClause)
            || HasAncestorKind(token.Parent, SyntaxKind.TypeArgument)
            || HasAncestorKind(token.Parent, SyntaxKind.TypeParameter)
            || HasAncestorKind(token.Parent, SyntaxKind.TypeConstraint)
            || HasAncestorKind(token.Parent, SyntaxKind.TypeParameterConstraint)
            || HasAncestorKind(token.Parent, SyntaxKind.TypeParameterConstraintClause)
            || HasAncestorKind(token.Parent, SyntaxKind.ArrayType)
            || HasAncestorKind(token.Parent, SyntaxKind.NullableType)
            || HasAncestorKind(token.Parent, SyntaxKind.PointerType)
            || HasAncestorKind(token.Parent, SyntaxKind.FunctionType)
            || HasAncestorKind(token.Parent, SyntaxKind.TupleType)
            || HasAncestorKind(token.Parent, SyntaxKind.ByRefType)
            || HasAncestorKind(token.Parent, SyntaxKind.UnionType)
            || HasAncestorKind(token.Parent, SyntaxKind.BaseList))
            return true;

        return token.Parent.Kind is SyntaxKind.ClassDeclaration
            or SyntaxKind.TypeDeclaration
            or SyntaxKind.BaseTypeDeclaration
            or SyntaxKind.InterfaceDeclaration
            or SyntaxKind.EnumDeclaration
            or SyntaxKind.UnionDeclaration
            or SyntaxKind.TypeParameter;
    }

    private static bool IsMethodIdentifier(SyntaxToken token)
    {
        if (token.Parent is null)
            return false;

        if (token.Parent.Kind is SyntaxKind.MethodDeclaration
            or SyntaxKind.FunctionStatement
            or SyntaxKind.ConstructorDeclaration
            or SyntaxKind.NamedConstructorDeclaration
            or SyntaxKind.ConversionOperatorDeclaration
            or SyntaxKind.OperatorDeclaration)
            return true;

        if (token.Parent.Kind is SyntaxKind.IdentifierName or SyntaxKind.GenericName)
        {
            var parent = token.Parent.Parent;
            if (parent?.Kind == SyntaxKind.InvocationExpression)
                return true;

            if (parent?.Kind == SyntaxKind.MemberAccessExpression && parent.Parent?.Kind == SyntaxKind.InvocationExpression)
                return true;
        }

        return false;
    }

    private static bool IsPropertyIdentifier(SyntaxToken token)
    {
        if (token.Parent is null)
            return false;

        if (token.Parent.Kind is SyntaxKind.PropertyDeclaration or SyntaxKind.IndexerDeclaration)
            return true;

        return token.Parent.Kind is SyntaxKind.IdentifierName or SyntaxKind.GenericName
            && token.Parent.Parent?.Kind == SyntaxKind.MemberAccessExpression;
    }

    private static bool IsEventIdentifier(SyntaxToken token)
        => token.Parent?.Kind == SyntaxKind.EventDeclaration;

    private static bool IsFieldIdentifier(SyntaxToken token)
        => token.Parent?.Kind == SyntaxKind.VariableDeclarator
            && HasAncestorKind(token.Parent, SyntaxKind.FieldDeclaration);

    private static bool IsParameterIdentifier(SyntaxToken token)
        => token.Parent?.Kind == SyntaxKind.Parameter;

    private static bool IsLocalIdentifier(SyntaxToken token)
        => token.Parent?.Kind == SyntaxKind.VariableDeclarator
            && (HasAncestorKind(token.Parent, SyntaxKind.LocalDeclarationStatement)
                || HasAncestorKind(token.Parent, SyntaxKind.UsingDeclarationStatement));

    private static bool HasAncestorKind(SyntaxNode? node, SyntaxKind kind)
    {
        var current = node;
        while (current is not null)
        {
            if (current.Kind == kind)
                return true;
            current = current.Parent;
        }

        return false;
    }

    private static SemanticClassification GetTriviaClassificationForKind(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.SingleLineCommentTrivia => SemanticClassification.Comment,
            SyntaxKind.MultiLineCommentTrivia => SemanticClassification.Comment,
            SyntaxKind.DocumentationCommentTrivia => SemanticClassification.Comment,
            _ => SemanticClassification.Default
        };
    }

    private static AnsiColor GetColorForClassification(SemanticClassification classification) => classification switch
    {
        SemanticClassification.Keyword => ColorScheme.Keyword,
        SemanticClassification.StringLiteral => ColorScheme.StringLiteral,
        SemanticClassification.Interpolation => ColorScheme.Interpolation,
        SemanticClassification.NumericLiteral => ColorScheme.NumericLiteral,
        SemanticClassification.Comment => ColorScheme.Comment,
        SemanticClassification.Method => ColorScheme.Method,
        SemanticClassification.Type => ColorScheme.Type,
        SemanticClassification.Namespace => ColorScheme.Namespace,
        SemanticClassification.Field => ColorScheme.Field,
        SemanticClassification.Parameter => ColorScheme.Parameter,
        SemanticClassification.Property => ColorScheme.Property,
        SemanticClassification.Local => ColorScheme.Local,
        SemanticClassification.Label => ColorScheme.Label,
        SemanticClassification.Event => ColorScheme.Event,
        SemanticClassification.NullableAnnotation => ColorScheme.NullableAnnotation,
        _ => ColorScheme.Default
    };

    private const string BoldStart = "\u001b[1m";
    private const string BoldEnd = "\u001b[22m";

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
