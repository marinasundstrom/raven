using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides completion items for a given position in a syntax tree.
/// </summary>
public class CompletionService
{
    internal static readonly ImmutableArray<string> BasicKeywords =
    [
        "if", "else", "while", "for", "return", "let", "var", "const", "new", "true", "false", "null"
    ];

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
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
            searchPosition--;
        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        try
        {
            var semanticModel = compilation.GetSemanticModel(syntaxTree);

            return CompletionProvider.GetCompletions(
                token,
                semanticModel,
                position,
                forceInsertionAtCaret: isWhitespaceOnlyLinePosition);
        }
        catch
        {
            // Keep completion usable in lightweight/editor scenarios where semantic setup
            // may fail (for example, missing metadata references).
            return GetBasicKeywordCompletions(token, position);
        }
    }

    private static bool IsWhitespaceOnlyLinePosition(string content, int position)
    {
        if ((uint)position > (uint)content.Length)
            return false;

        for (var i = position - 1; i >= 0; i--)
        {
            var ch = content[i];
            if (ch is ' ' or '\t')
                continue;

            if (ch is '\r' or '\n')
                return true;

            return false;
        }

        return true;
    }

    /// <summary>
    /// Gets the completion items available at the specified position asynchronously.
    /// </summary>
    /// <param name="compilation">The compilation that contains the syntax tree.</param>
    /// <param name="syntaxTree">The syntax tree being queried.</param>
    /// <param name="position">The position within the syntax tree.</param>
    /// <param name="cancellationToken">Token used to cancel the operation.</param>
    /// <returns>A materialized set of completion items applicable at the position.</returns>
    public async Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        CancellationToken cancellationToken = default)
    {
        return await Task.Run(
                () => GetCompletions(compilation, syntaxTree, position).ToImmutableArray(),
                cancellationToken)
            .ConfigureAwait(false);
    }

    internal static IEnumerable<CompletionItem> GetBasicKeywordCompletions(SyntaxToken token, int position)
    {
        var prefix = token.IsKind(SyntaxKind.IdentifierToken)
            ? token.ValueText
            : token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false } identifierName
                ? identifierName.Identifier.ValueText
                : string.Empty;

        var replacementSpan = token.IsKind(SyntaxKind.IdentifierToken)
            ? token.Span
            : token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false } identifier
                ? identifier.Identifier.Span
                : new TextSpan(position, 0);

        return BasicKeywords
            .Where(k => string.IsNullOrEmpty(prefix) || k.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            .Select(keyword => new CompletionItem(
                DisplayText: keyword,
                InsertionText: keyword,
                ReplacementSpan: replacementSpan));
    }
}
