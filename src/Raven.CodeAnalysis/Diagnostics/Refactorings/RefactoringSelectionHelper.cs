using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class RefactoringSelectionHelper
{
    public static bool TryGetSelectedNode(
        CodeRefactoringContext context,
        out SyntaxTree syntaxTree,
        out SyntaxNode root,
        out SyntaxNode node)
    {
        syntaxTree = null!;
        root = null!;
        node = null!;

        syntaxTree = context.Document.GetSyntaxTreeAsync(context.CancellationToken).GetAwaiter().GetResult();
        root = syntaxTree?.GetRoot(context.CancellationToken)!;
        if (syntaxTree is null || root is null || root.FullSpan.Length == 0)
            return false;

        var position = Math.Clamp(context.Span.Start, root.FullSpan.Start, root.FullSpan.End - 1);
        node = root.FindToken(position).Parent!;
        return node is not null;
    }

    public static bool IntersectsSelection(CodeRefactoringContext context, TextSpan candidateSpan)
    {
        var selection = context.Span;
        if (selection.Length == 0)
            return candidateSpan.Start <= selection.Start && selection.Start <= candidateSpan.End;

        return selection.Start < candidateSpan.End && candidateSpan.Start < selection.End;
    }
}
