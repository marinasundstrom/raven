using System;
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
        // Completion requests typically originate from the caret position which
        // lies *after* the character that triggered completion.  Because
        // FindToken looks for the token that contains the provided position and
        // token spans are end-exclusive, passing the caret position directly
        // results in the token to the right being returned.  Adjust the search
        // position to ensure the token to the left of the caret is used.
        var searchPosition = Math.Max(0, position - 1);
        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        return CompletionProvider.GetCompletions(token, semanticModel, position);
    }
}
