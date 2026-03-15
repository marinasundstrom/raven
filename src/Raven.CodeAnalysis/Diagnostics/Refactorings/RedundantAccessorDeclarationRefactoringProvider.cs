using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Diagnostics;

public sealed class RedundantAccessorDeclarationRefactoringProvider : CodeRefactoringProvider
{
    public override void RegisterRefactorings(CodeRefactoringContext context)
    {
        if (!RefactoringSelectionHelper.TryGetSelectedNode(context, out _, out _, out var node))
            return;

        var accessorList = node.FirstAncestorOrSelf<AccessorListSyntax>();
        if (accessorList is null || !RefactoringSelectionHelper.IntersectsSelection(context, accessorList.Span))
            return;

        var isRedundant = accessorList.Parent switch
        {
            PropertyDeclarationSyntax property => RedundantAccessorDeclarationAnalyzer.TryGetRedundantAccessorList(
                property.BindingKeyword.Kind,
                property.AccessorList,
                out _),
            IndexerDeclarationSyntax indexer => RedundantAccessorDeclarationAnalyzer.TryGetRedundantAccessorList(
                indexer.BindingKeyword.Kind,
                indexer.AccessorList,
                out _),
            _ => false
        };

        if (!isRedundant)
            return;

        var span = accessorList.Span;
        var sourceText = context.Document.GetTextAsync(context.CancellationToken).GetAwaiter().GetResult().ToString();
        if (span.Start > 0 && sourceText[span.Start - 1] == ' ')
            span = new TextSpan(span.Start - 1, span.Length + 1);

        context.RegisterRefactoring(
            CodeAction.CreateTextChange(
                "Remove redundant accessor list",
                context.Document.Id,
                new TextChange(span, string.Empty)));
    }
}
