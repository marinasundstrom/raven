using System.Text;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class StringConcatenationRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out _, out _, out var node))
            return;

        var concat = node as InfixOperatorExpressionSyntax
            ?? node.AncestorsAndSelf().OfType<InfixOperatorExpressionSyntax>().FirstOrDefault();
        if (concat is null || concat.Kind != SyntaxKind.AddExpression)
            return;

        var topmostConcat = StringConcatenationToInterpolatedStringCodeFixProvider.GetTopmostConcat(concat);
        if (!RefactoringSelectionHelper.IntersectsSelection(context, topmostConcat.Span))
            return;

        var semanticModel = context.Document.GetSemanticModelAsync(context.CancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null || !StringConcatenationAnalyzer.IsStringType(semanticModel.GetTypeInfo(topmostConcat).Type))
            return;

        var parts = new List<ExpressionSyntax>();
        StringConcatenationToInterpolatedStringCodeFixProvider.FlattenConcat(topmostConcat, parts);
        if (parts.Count < 2)
            return;

        if (parts.All(part => StringConcatenationAnalyzer.CanMergeIntoStringText(part, semanticModel)))
        {
            var mergedBuilder = new StringBuilder();
            foreach (var part in parts)
            {
                if (!MergeStringLiteralConcatenationCodeFixProvider.TryGetTextPart(part, semanticModel, out var text))
                    return;

                mergedBuilder.Append(text);
            }

            var mergedReplacement = "\"" + MergeStringLiteralConcatenationCodeFixProvider.EscapeStringLiteral(mergedBuilder.ToString()) + "\"";
            context.RegisterRefactoring(
                CodeAction.CreateTextChange(
                    "Merge string concatenation",
                    context.Document.Id,
                    new TextChange(topmostConcat.Span, mergedReplacement)));
            return;
        }

        if (!StringConcatenationToInterpolatedStringCodeFixProvider.TryBuildInterpolatedString(parts, semanticModel, out var replacementText))
            return;

        context.RegisterRefactoring(
            CodeAction.CreateTextChange(
                "Convert to interpolated string",
                context.Document.Id,
                new TextChange(topmostConcat.Span, replacementText)));
    }
}
