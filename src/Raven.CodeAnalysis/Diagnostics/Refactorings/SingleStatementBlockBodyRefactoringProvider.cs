using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class SingleStatementBlockBodyRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out _, out _, out var node))
            return;

        var block = SingleStatementBlockBodyCodeFixProvider.TryGetBodyBlock(node) ?? node.FirstAncestorOrSelf<BlockStatementSyntax>();
        var declaration = block?.Parent;
        if (block is null ||
            (declaration is null && !RefactoringSelectionHelper.IntersectsSelection(context, block.Span)) ||
            (declaration is not null &&
             !RefactoringSelectionHelper.IntersectsSelection(context, block.Span) &&
             !RefactoringSelectionHelper.IntersectsSelection(context, declaration.Span)))
        {
            return;
        }

        if (!SingleStatementBlockBodyAnalyzer.TryGetConvertibleExpression(block, out var expression))
            return;

        context.RegisterRefactoring(
            CodeAction.CreateTextChange(
                "Convert to expression body",
                context.Document.Id,
                new TextChange(block.Span, $"=> {expression}")));
    }
}
