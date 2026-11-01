namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxListHelpers
{
    public static bool IsSoundSeparatedList<TNode>(this SeparatedSyntaxList<TNode> list, SyntaxKind expectedSeparatorKind)
        where TNode : SyntaxNode
    {
        var expectNode = true;
        foreach (var item in list.GetWithSeparators())
        {
            if (expectNode)
            {
                if (item.IsToken || item.AsNode() is not TNode)
                    return false;
            }
            else if (!item.IsToken || item.AsToken().Kind != expectedSeparatorKind)
            {
                return false;
            }

            expectNode = !expectNode;
        }

        // Empty lists and a trailing separator are both valid representations.
        return true;
    }
}
