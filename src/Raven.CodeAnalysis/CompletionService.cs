using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides completion items for a given position in a syntax tree.
/// </summary>
public class CompletionService
{
    /// <summary>
    /// Gets the completion items available at the specified position.
    /// </summary>
    /// <param name="compilation">The compilation that contains the syntax tree.</param>
    /// <param name="syntaxTree">The syntax tree being queried.</param>
    /// <param name="position">The position within the syntax tree.</param>
    /// <returns>A sequence of completion items applicable at the position.</returns>
    public IEnumerable<CompletionItem> GetCompletions(Compilation compilation, SyntaxTree syntaxTree, int position)
    {
        var token = syntaxTree.GetRoot().FindToken(position);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        return CompletionProvider.GetCompletions(token, semanticModel, position);
    }
}
