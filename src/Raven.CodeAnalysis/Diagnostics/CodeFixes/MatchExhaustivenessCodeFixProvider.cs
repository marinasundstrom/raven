using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class MatchExhaustivenessCodeFixProvider : CodeFixProvider
{
    private const string ThrowPlaceholderExpression = "throw System.NotImplementedException()";

    private static readonly ImmutableArray<string> FixableIds =
    [
        CompilerDiagnostics.MatchExpressionNotExhaustive.Id,
        CompilerDiagnostics.MatchExpressionCatchAllRedundant.Id
    ];

    public override IEnumerable<string> FixableDiagnosticIds => FixableIds;

    public override void RegisterCodeFixes(CodeFixContext context)
    {
        var diagnostic = context.Diagnostic;
        if (!diagnostic.Location.IsInSource)
            return;

        if (string.Equals(diagnostic.Id, CompilerDiagnostics.MatchExpressionNotExhaustive.Id, StringComparison.OrdinalIgnoreCase))
        {
            RegisterAddMissingArmFix(context, diagnostic);
            return;
        }

        if (string.Equals(diagnostic.Id, CompilerDiagnostics.MatchExpressionCatchAllRedundant.Id, StringComparison.OrdinalIgnoreCase))
            RegisterReplaceCatchAllFix(context, diagnostic);
    }

    private static void RegisterAddMissingArmFix(CodeFixContext context, Diagnostic diagnostic)
    {
        if (!TryGetMissingCase(diagnostic, out var missingCase))
            return;

        var syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        var root = syntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var matchExpression = FindMatchExpression(root, diagnostic.Location.SourceSpan);
        if (matchExpression is null)
            return;

        var semanticModel = context.Document.GetSemanticModelAsync(context.CancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null)
            return;

        var patternText = FormatPatternText(missingCase, semanticModel, matchExpression);
        var sourceText = context.Document.GetTextAsync(context.CancellationToken).GetAwaiter().GetResult();
        var text = sourceText.ToString();
        var armText = CreateMissingArmText(text, matchExpression, patternText);
        var insertionPosition = GetLineStart(text, GetMatchExpressionCloseBraceToken(matchExpression).Span.Start);
        var change = new TextChange(new TextSpan(insertionPosition, 0), armText);

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                $"Add missing match arm for '{patternText}'",
                context.Document.Id,
                change));
    }

    private static void RegisterReplaceCatchAllFix(CodeFixContext context, Diagnostic diagnostic)
    {
        var syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        var root = syntaxTree?.GetRoot(context.CancellationToken);
        if (root is null)
            return;

        var token = root.FindToken(diagnostic.Location.SourceSpan.Start);
        var arm = token.Parent?.FirstAncestorOrSelf<MatchArmSyntax>();
        var matchExpression = FindContainingMatchExpression(arm);
        if (arm is null || matchExpression is null)
            return;

        var semanticModel = context.Document.GetSemanticModelAsync(context.CancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null)
            return;

        var ignoreCatchAllInfo = GetMatchExhaustiveness(
            semanticModel,
            matchExpression,
            new MatchExhaustivenessOptions(ignoreCatchAllPatterns: true));

        if (!TryGetSingleMissingCase(ignoreCatchAllInfo.MissingCases, out var missingCase))
        {
            RegisterRemoveCatchAllFix(context, arm);
            return;
        }

        if (missingCase == "_")
        {
            RegisterRemoveCatchAllFix(context, arm);
            return;
        }

        var patternText = FormatPatternText(missingCase, semanticModel, matchExpression);
        var change = new TextChange(arm.Pattern.Span, patternText);

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                $"Replace catch-all with '{patternText}'",
                context.Document.Id,
                change));
    }

    private static void RegisterRemoveCatchAllFix(CodeFixContext context, MatchArmSyntax arm)
    {
        var sourceText = context.Document.GetTextAsync(context.CancellationToken).GetAwaiter().GetResult();
        var text = sourceText.ToString();
        var span = GetLineRemovalSpan(text, arm.Span);
        if (span.Length == 0)
            return;

        context.RegisterCodeFix(
            CodeAction.CreateTextChange(
                "Remove redundant catch-all arm",
                context.Document.Id,
                new TextChange(span, string.Empty)));
    }

    private static SyntaxNode? FindMatchExpression(SyntaxNode root, TextSpan diagnosticSpan)
    {
        var token = root.FindToken(diagnosticSpan.Start);
        return FindContainingMatchExpression(token.Parent);
    }

    private static bool TryGetMissingCase(Diagnostic diagnostic, out string missingCase)
    {
        missingCase = string.Empty;

        var args = diagnostic.GetMessageArgs();
        if (args.Length == 0 || args[0] is not string value || string.IsNullOrWhiteSpace(value))
            return false;

        missingCase = NormalizeMissingCase(value);
        return true;
    }

    private static string NormalizeMissingCase(string value)
    {
        const string prefix = "Missing match case:";

        value = value.Trim();
        if (!value.StartsWith(prefix, StringComparison.Ordinal))
            return value;

        var firstQuote = value.IndexOf('\'');
        var lastQuote = value.LastIndexOf('\'');
        if (firstQuote >= 0 && lastQuote > firstQuote)
            return value.Substring(firstQuote + 1, lastQuote - firstQuote - 1);

        return value.Substring(prefix.Length).Trim().TrimEnd('.');
    }

    private static bool TryGetSingleMissingCase(ImmutableArray<string> missingCases, out string missingCase)
    {
        missingCase = string.Empty;

        if (missingCases.Length != 1)
            return false;

        missingCase = missingCases[0];
        return true;
    }

    private static string FormatPatternText(
        string missingCase,
        SemanticModel semanticModel,
        SyntaxNode matchExpression)
    {
        if (missingCase is "_" or "null" or "true" or "false" || missingCase.StartsWith(".", StringComparison.Ordinal))
            return missingCase;

        var scrutineeType = semanticModel.GetTypeInfo(GetMatchExpressionScrutinee(matchExpression)).Type;
        var union = scrutineeType.TryGetUnion() ?? scrutineeType.TryGetUnionCase()?.Union;
        if (union is not null)
        {
            var caseType = union.DeclaredCaseTypes.FirstOrDefault(candidate =>
                string.Equals(candidate.Name, missingCase, StringComparison.Ordinal));
            if (caseType is not null)
            {
                return caseType.ConstructorParameters.Length == 0
                    ? "." + missingCase
                    : $".{missingCase}({string.Join(", ", Enumerable.Repeat("_", caseType.ConstructorParameters.Length))})";
            }
        }

        if (scrutineeType is INamedTypeSymbol { TypeKind: TypeKind.Enum } enumType &&
            enumType.GetMembers().OfType<IFieldSymbol>().Any(field =>
                field.IsConst &&
                field.ContainingType?.TypeKind == TypeKind.Enum &&
                string.Equals(field.Name, missingCase, StringComparison.Ordinal)))
        {
            return "." + missingCase;
        }

        return missingCase;
    }

    private static string CreateMissingArmText(string sourceText, SyntaxNode matchExpression, string patternText)
    {
        var newLine = sourceText.Contains("\r\n", StringComparison.Ordinal) ? "\r\n" : "\n";
        var armIndent = GetArmIndent(sourceText, matchExpression);

        return $"{armIndent}{patternText} => {ThrowPlaceholderExpression}{newLine}";
    }

    private static string GetArmIndent(string sourceText, SyntaxNode matchExpression)
    {
        var arms = GetMatchExpressionArms(matchExpression);
        if (arms.Count > 0)
            return GetLineIndent(sourceText, arms[0].Span.Start);

        return GetLineIndent(sourceText, GetMatchExpressionCloseBraceToken(matchExpression).Span.Start) + "    ";
    }

    private static SyntaxNode? FindContainingMatchExpression(SyntaxNode? node)
        => node?.FirstAncestorOrSelf<MatchExpressionSyntax>()
            ?? (SyntaxNode?)node?.FirstAncestorOrSelf<PostfixMatchExpressionSyntax>();

    private static MatchExhaustivenessInfo GetMatchExhaustiveness(
        SemanticModel semanticModel,
        SyntaxNode matchExpression,
        MatchExhaustivenessOptions options)
        => matchExpression switch
        {
            MatchExpressionSyntax keywordFirst => semanticModel.GetMatchExhaustiveness(keywordFirst, options),
            PostfixMatchExpressionSyntax postfix => semanticModel.GetMatchExhaustiveness(postfix, options),
            _ => new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false),
        };

    private static ExpressionSyntax GetMatchExpressionScrutinee(SyntaxNode matchExpression)
        => matchExpression switch
        {
            MatchExpressionSyntax keywordFirst => keywordFirst.Expression,
            PostfixMatchExpressionSyntax postfix => postfix.Expression,
            _ => throw new ArgumentException("Expected match expression syntax.", nameof(matchExpression)),
        };

    private static SyntaxList<MatchArmSyntax> GetMatchExpressionArms(SyntaxNode matchExpression)
        => matchExpression switch
        {
            MatchExpressionSyntax keywordFirst => keywordFirst.Arms,
            PostfixMatchExpressionSyntax postfix => postfix.Arms,
            _ => default,
        };

    private static SyntaxToken GetMatchExpressionCloseBraceToken(SyntaxNode matchExpression)
        => matchExpression switch
        {
            MatchExpressionSyntax keywordFirst => keywordFirst.CloseBraceToken,
            PostfixMatchExpressionSyntax postfix => postfix.CloseBraceToken,
            _ => default,
        };

    private static string GetLineIndent(string sourceText, int position)
    {
        var lineStart = sourceText.LastIndexOf('\n', Math.Max(0, position - 1));
        lineStart = lineStart < 0 ? 0 : lineStart + 1;

        var index = lineStart;
        while (index < sourceText.Length && (sourceText[index] == ' ' || sourceText[index] == '\t'))
            index++;

        return sourceText.Substring(lineStart, index - lineStart);
    }

    private static int GetLineStart(string sourceText, int position)
    {
        var lineStart = sourceText.LastIndexOf('\n', Math.Max(0, position - 1));
        return lineStart < 0 ? 0 : lineStart + 1;
    }

    private static TextSpan GetLineRemovalSpan(string sourceText, TextSpan span)
    {
        var start = sourceText.LastIndexOf('\n', Math.Max(0, span.Start - 1));
        start = start < 0 ? 0 : start + 1;

        var end = sourceText.IndexOf('\n', span.End);
        if (end >= 0)
            end++;
        else
            end = span.End;

        return new TextSpan(start, Math.Max(0, end - start));
    }
}
