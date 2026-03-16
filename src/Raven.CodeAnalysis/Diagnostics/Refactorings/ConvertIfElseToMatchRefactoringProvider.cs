using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ConvertIfElseToMatchRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out _, out _, out var node))
            return;

        var ifStatement = node.FirstAncestorOrSelf<IfStatementSyntax>();
        var ifPatternStatement = node.FirstAncestorOrSelf<IfPatternStatementSyntax>();
        var statement = (SyntaxNode?)ifPatternStatement ?? ifStatement;
        if (statement is null || !RefactoringSelectionHelper.IntersectsSelection(context, statement.Span))
            return;

        var sourceText = context.Document.GetTextAsync(context.CancellationToken).GetAwaiter().GetResult();
        string expressionText;
        string patternText;
        string fallbackPatternText;
        StatementSyntax thenStatement;
        StatementSyntax elseStatement;

        if (ifPatternStatement is not null)
        {
            if (ifPatternStatement.ElseClause is null)
                return;

            expressionText = sourceText.GetSubText(ifPatternStatement.Expression.Span);
            var rawPatternText = sourceText.GetSubText(ifPatternStatement.Pattern.Span);
            patternText = ifPatternStatement.BindingKeyword.Kind == SyntaxKind.None
                ? rawPatternText
                : $"{ifPatternStatement.BindingKeyword.Text} {rawPatternText}";
            fallbackPatternText = GetFallbackPatternText(ifPatternStatement.Pattern);
            thenStatement = ifPatternStatement.ThenStatement;
            elseStatement = ifPatternStatement.ElseClause.Statement;
        }
        else
        {
            if (ifStatement is null ||
                ifStatement.ElseClause is null ||
                ifStatement.Condition is not IsPatternExpressionSyntax isPattern)
            {
                return;
            }

            expressionText = sourceText.GetSubText(isPattern.Expression.Span);
            patternText = sourceText.GetSubText(isPattern.Pattern.Span);
            fallbackPatternText = GetFallbackPatternText(isPattern.Pattern);
            thenStatement = ifStatement.ThenStatement;
            elseStatement = ifStatement.ElseClause.Statement;
        }

        var indent = GetIndentation(sourceText, statement.Span.Start);
        var thenText = IndentStatementText(sourceText.GetSubText(thenStatement.Span), indent);
        var elseText = IndentStatementText(sourceText.GetSubText(elseStatement.Span), indent);

        var replacement =
            $"match {expressionText} {{{Environment.NewLine}" +
            $"{indent}    {patternText} => {thenText}{Environment.NewLine}" +
            $"{indent}    {fallbackPatternText} => {elseText}{Environment.NewLine}" +
            $"{indent}}}{Environment.NewLine}";

        context.RegisterRefactoring(
            CodeAction.CreateTextChange(
                "Convert if/else to match",
                context.Document.Id,
                new TextChange(statement.Span, replacement)));
    }

    private static string GetIndentation(SourceText sourceText, int position)
    {
        var lineStart = position;
        while (lineStart > 0)
        {
            var previous = sourceText.GetSubText(lineStart - 1, 1)[0];
            if (previous is '\n' or '\r')
                break;

            lineStart--;
        }

        var indentEnd = lineStart;
        while (indentEnd < sourceText.Length)
        {
            var current = sourceText.GetSubText(indentEnd, 1)[0];
            if (current is not (' ' or '\t'))
                break;

            indentEnd++;
        }

        return sourceText.GetSubText(lineStart, indentEnd - lineStart);
    }

    private static string IndentStatementText(string statementText, string indent)
    {
        var newLine = statementText.Contains("\r\n", StringComparison.Ordinal) ? "\r\n" : "\n";
        return statementText.Replace(newLine, $"{newLine}{indent}", StringComparison.Ordinal);
    }

    private static string GetFallbackPatternText(PatternSyntax matchedPattern)
    {
        var matchedCaseName = TryGetMatchedCaseName(matchedPattern);
        if (TryGetComplementaryCaseName(matchedCaseName) is { } fallbackCaseName)
            return fallbackCaseName;

        return "_";
    }

    private static string? TryGetComplementaryCaseName(string? matchedCaseName)
    {
        return matchedCaseName switch
        {
            "Some" => "None",
            "None" => "Some",
            "Ok" => "Error",
            "Error" => "Ok",
            _ => null,
        };
    }

    private static string? TryGetMatchedCaseName(PatternSyntax pattern)
    {
        return pattern switch
        {
            MemberPatternSyntax memberPattern => memberPattern.Path.Identifier.ValueText,
            NominalDeconstructionPatternSyntax nominalPattern => GetLastTypeIdentifier(nominalPattern.Type),
            ConstantPatternSyntax { Expression: IdentifierNameSyntax identifierName } => identifierName.Identifier.ValueText,
            ConstantPatternSyntax { Expression: GenericNameSyntax genericName } => genericName.Identifier.ValueText,
            _ => null,
        };
    }

    private static string? GetLastTypeIdentifier(TypeSyntax type)
    {
        return type switch
        {
            IdentifierNameSyntax identifierName => identifierName.Identifier.ValueText,
            GenericNameSyntax genericName => genericName.Identifier.ValueText,
            QualifiedNameSyntax qualifiedName => GetLastTypeIdentifier(qualifiedName.Right),
            _ => null,
        };
    }
}
