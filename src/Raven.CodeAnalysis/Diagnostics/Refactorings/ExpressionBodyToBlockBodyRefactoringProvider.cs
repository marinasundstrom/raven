using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class ExpressionBodyToBlockBodyRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out var syntaxTree, out _, out var node))
            return;

        var semanticModel = context.Document.GetSemanticModelAsync(context.CancellationToken).GetAwaiter().GetResult();
        if (semanticModel is null)
            return;

        var declaration = ExpressionBodyToBlockBodyCodeFixProvider.FindDeclaration(node);
        if (declaration is null ||
            !ExpressionBodyToBlockBodyCodeFixProvider.TryGetExpressionBody(declaration, out var expressionBody) ||
            !RefactoringSelectionHelper.IntersectsSelection(context, expressionBody.Span))
        {
            return;
        }

        var replacement = ExpressionBodyToBlockBodyCodeFixProvider.BuildBlockBodyText(
            declaration,
            expressionBody.Expression,
            semanticModel,
            syntaxTree.GetText());
        var replacementSpan = ExpressionBodyToBlockBodyCodeFixProvider.GetExpressionBodyReplacementSpan(declaration, expressionBody);

        context.RegisterRefactoring(
            CodeAction.CreateTextChange(
                "Convert to block body",
                context.Document.Id,
                new TextChange(replacementSpan, replacement)));
    }
}
