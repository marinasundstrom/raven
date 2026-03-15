using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class TargetTypedUnionCaseRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out _, out _, out var node))
            return;

        var semanticModel = context.Document.GetSemanticModelAsync(context.CancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null)
            return;

        if (node.FirstAncestorOrSelf<LocalDeclarationStatementSyntax>() is { } localDeclaration &&
            RefactoringSelectionHelper.IntersectsSelection(context, localDeclaration.Span) &&
            PreferTargetTypedUnionCaseAnalyzer.TryCreateSuggestion(localDeclaration, semanticModel, out var declarationSuggestion))
        {
            context.RegisterRefactoring(
                CodeAction.CreateTextChange(
                    "Rewrite declaration using target-typed union case construction",
                    context.Document.Id,
                    new TextChange(localDeclaration.EffectiveSpan, declarationSuggestion.RewrittenStatementText)));
        }

        foreach (var expression in node.AncestorsAndSelf().OfType<ExpressionSyntax>())
        {
            if (!RefactoringSelectionHelper.IntersectsSelection(context, expression.Span))
                continue;

            if (!PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.TryCreateSuggestion(expression, semanticModel, out var expressionSuggestion))
                continue;

            context.RegisterRefactoring(
                CodeAction.CreateTextChange(
                    "Rewrite to target-typed union case syntax",
                    context.Document.Id,
                    new TextChange(expression.Span, expressionSuggestion.RewrittenExpressionText)));
            break;
        }
    }
}
