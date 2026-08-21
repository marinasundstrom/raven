using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroCompletionService
{
    public static bool TryGetCompletions(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        int position,
        CancellationToken cancellationToken,
        out ImmutableArray<CompletionItem> completions)
        => TryGetCompletions(
            semanticModel,
            syntax,
            syntax,
            position,
            cancellationToken,
            out completions);

    public static bool TryGetCompletions(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        SyntaxNode resolutionContext,
        int position,
        CancellationToken cancellationToken,
        out ImmutableArray<CompletionItem> completions)
    {
        completions = ImmutableArray<CompletionItem>.Empty;
        cancellationToken.ThrowIfCancellationRequested();

        if (!FreestandingMacroInvocation.TryCreate(syntax, out var invocation) ||
            invocation.TokenTree is null ||
            !invocation.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                resolutionContext,
                name,
                out var loaded,
                out _) ||
            !loaded.Descriptor.HasTokenBody ||
            loaded.Macro is not IMacroCompletionProvider provider)
        {
            return false;
        }

        var context = new TokenTreeMacroContext(
            semanticModel.Compilation,
            semanticModel,
            invocation,
            loaded.Macro,
            cancellationToken);
        if (position < context.BodySpan.Start || position > context.BodySpan.End)
            return false;

        try
        {
            var bodyRelativePosition = position - context.BodySpan.Start;
            var provided = provider.GetCompletions(context, bodyRelativePosition);
            cancellationToken.ThrowIfCancellationRequested();
            if (provided.IsDefaultOrEmpty)
                return true;

            completions = provided
                .Where(item =>
                    item is not null &&
                    item.BodyRelativeReplacementSpan.Start >= 0 &&
                    item.BodyRelativeReplacementSpan.End <= context.BodySpan.Length)
                .Select(item => new CompletionItem(
                    item.DisplayText,
                    item.InsertionText,
                    new TextSpan(
                        context.BodySpan.Start + item.BodyRelativeReplacementSpan.Start,
                        item.BodyRelativeReplacementSpan.Length),
                    item.CursorOffset,
                    item.Description,
                    item.Symbol))
                .Distinct()
                .OrderBy(static item => item.DisplayText, StringComparer.OrdinalIgnoreCase)
                .ThenBy(static item => item.DisplayText, StringComparer.Ordinal)
                .ToImmutableArray();
            return true;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch
        {
            // Optional macro tooling must not break ordinary editor requests.
            completions = ImmutableArray<CompletionItem>.Empty;
            return true;
        }
    }
}
