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
    internal readonly record struct CompletionComputation(
        ImmutableArray<CompletionItem> Items,
        double SemanticModelMs,
        double ProviderMs,
        bool UsedFallback,
        string? FailureType);

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

            return GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition);
        }
        catch
        {
            // Keep completion usable in lightweight/editor scenarios where semantic setup
            // may fail (for example, missing metadata references).
            return GetBasicKeywordCompletions(token, position);
        }
    }

    public IEnumerable<CompletionItem> GetCompletions(SemanticModel semanticModel, int position)
    {
        var syntaxTree = semanticModel.SyntaxTree;
        var searchPosition = Math.Max(0, position - 1);
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
        {
            searchPosition--;
        }

        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        try
        {
            return GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition);
        }
        catch
        {
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
        return (await GetCompletionsWithMetricsAsync(compilation, syntaxTree, position, cancellationToken).ConfigureAwait(false)).Items;
    }

    public async Task<ImmutableArray<CompletionItem>> GetCompletionsAsync(
        SemanticModel semanticModel,
        int position,
        CancellationToken cancellationToken = default)
    {
        return (await GetCompletionsWithMetricsAsync(semanticModel, position, cancellationToken).ConfigureAwait(false)).Items;
    }

    internal async Task<CompletionComputation> GetCompletionsWithMetricsAsync(
        Compilation compilation,
        SyntaxTree syntaxTree,
        int position,
        CancellationToken cancellationToken = default)
    {
        return await Task.Run(
                () => GetCompletionsWithMetrics(compilation, syntaxTree, position),
                cancellationToken)
            .ConfigureAwait(false);
    }

    internal async Task<CompletionComputation> GetCompletionsWithMetricsAsync(
        SemanticModel semanticModel,
        int position,
        CancellationToken cancellationToken = default)
    {
        return await Task.Run(
                () => GetCompletionsWithMetrics(semanticModel, position),
                cancellationToken)
            .ConfigureAwait(false);
    }

    internal CompletionComputation GetCompletionsWithMetrics(Compilation compilation, SyntaxTree syntaxTree, int position)
    {
        var semanticModelMs = 0d;
        var semanticModelStopwatch = System.Diagnostics.Stopwatch.StartNew();
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        semanticModelStopwatch.Stop();
        semanticModelMs = semanticModelStopwatch.Elapsed.TotalMilliseconds;

        return GetCompletionsWithMetrics(semanticModel, position, semanticModelMs);
    }

    internal CompletionComputation GetCompletionsWithMetrics(SemanticModel semanticModel, int position)
        => GetCompletionsWithMetrics(semanticModel, position, semanticModelMs: 0d);

    private CompletionComputation GetCompletionsWithMetrics(
        SemanticModel semanticModel,
        int position,
        double semanticModelMs)
    {
        var searchPosition = Math.Max(0, position - 1);
        var syntaxTree = semanticModel.SyntaxTree;
        var sourceText = syntaxTree.GetText();
        var content = sourceText.ToString();
        var isWhitespaceOnlyLinePosition = IsWhitespaceOnlyLinePosition(content, position);
        while (searchPosition > 0 &&
               searchPosition < content.Length &&
               char.IsWhiteSpace(content[searchPosition]))
        {
            searchPosition--;
        }

        var token = syntaxTree.GetRoot().FindToken(searchPosition);
        var providerMs = 0d;

        try
        {
            var providerStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var items = GetCompletions(token, semanticModel, position, isWhitespaceOnlyLinePosition)
                .ToImmutableArray();
            providerStopwatch.Stop();
            providerMs = providerStopwatch.Elapsed.TotalMilliseconds;

            return new CompletionComputation(items, semanticModelMs, providerMs, UsedFallback: false, FailureType: null);
        }
        catch (Exception ex)
        {
            var fallbackStopwatch = System.Diagnostics.Stopwatch.StartNew();
            var items = GetBasicKeywordCompletions(token, position).ToImmutableArray();
            fallbackStopwatch.Stop();
            providerMs += fallbackStopwatch.Elapsed.TotalMilliseconds;
            return new CompletionComputation(items, semanticModelMs, providerMs, UsedFallback: true, ex.GetType().Name);
        }
    }

    private static IEnumerable<CompletionItem> GetCompletions(
        SyntaxToken token,
        SemanticModel semanticModel,
        int position,
        bool forceInsertionAtCaret)
    {
        return CompletionProvider.GetCompletions(
            token,
            semanticModel,
            position,
            forceInsertionAtCaret: forceInsertionAtCaret);
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
