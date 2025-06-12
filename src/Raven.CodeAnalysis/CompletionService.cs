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