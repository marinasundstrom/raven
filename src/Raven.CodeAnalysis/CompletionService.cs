using System;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class CompletionService
{
    public IEnumerable<CompletionItem> GetCompletions(Compilation compilation, SyntaxTree syntaxTree, int position)
    {
        var token = syntaxTree.GetRoot().FindToken(position);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        return CompletionProvider.GetCompletions(token, semanticModel, position);
    }
}

public static class SyntaxListHelpers
{

    public static bool IsSoundSeparatedList<TNode>(this SeparatedSyntaxList<TNode> list, SyntaxKind expectedSeparatorKind)
    where TNode : SyntaxNode
    {
        var items = list.GetWithSeparators().ToArray();

        for (int i = 0; i < list.Count; i++)
        {
            var item = items[i];

            if (i % 2 == 0)
            {
                // Expect node
                if (item.IsToken || item.AsNode() is not TNode)
                    return false;
            }
            else
            {
                // Expect separator
                if (!item.IsToken || item.AsToken().Kind != expectedSeparatorKind)
                    return false;
            }
        }

        int nodeCount = (list.Count + 1) / 2;
        int separatorCount = list.Count / 2;

        return list.SeparatorCount == separatorCount &&
               list.GetWithSeparators().Count() == list.Count &&
               list.Count == nodeCount;
    }
}