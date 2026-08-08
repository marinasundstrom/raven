using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroTokenInfoService
{
    public static ImmutableArray<MacroTokenInfo> GetTokens(
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax expression,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (expression.TokenTree is null ||
            !expression.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                expression,
                name,
                out var loaded,
                out _) ||
            loaded.Macro is not ITokenTreeExpressionMacro tokenTreeMacro)
        {
            return ImmutableArray<MacroTokenInfo>.Empty;
        }

        try
        {
            var context = new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                expression,
                tokenTreeMacro,
                cancellationToken);
            if (loaded.Macro is IMacroExpansionMetadataProvider)
            {
                var contributed = semanticModel.GetMacroExpansion(expression, cancellationToken)?.TokenInfos ??
                    ImmutableArray<MacroTokenInfo>.Empty;
                if (!contributed.IsDefaultOrEmpty)
                    return NormalizeContributedTokens(context, contributed);
            }

            var classifier = loaded.Macro as IMacroTokenClassifier;
            var kindProvider = loaded.Macro as IMacroTokenKindProvider;
            var symbolProvider = loaded.Macro as IMacroTokenSymbolProvider;
            var stream = context.CreateTokenStream();
            var builder = ImmutableArray.CreateBuilder<MacroTokenInfo>();

            while (!stream.IsEndOfFile)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var token = stream.ReadToken();
                var kindName = GetKindName(kindProvider, token);
                var classification = GetClassification(classifier, context, token);
                var symbol = GetSymbol(symbolProvider, context, token);

                builder.Add(context.CreateTokenInfo(token, kindName, classification, symbol));
            }

            return builder.ToImmutable();
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch
        {
            return ImmutableArray<MacroTokenInfo>.Empty;
        }
    }

    private static ImmutableArray<MacroTokenInfo> NormalizeContributedTokens(
        TokenTreeMacroContext context,
        ImmutableArray<MacroTokenInfo> tokens)
        => tokens
            .Where(token =>
                token is not null &&
                token.BodyRelativeSpan.Start >= 0 &&
                token.BodyRelativeSpan.End <= context.BodySpan.Length &&
                token.Span == new TextSpan(
                    context.BodySpan.Start + token.BodyRelativeSpan.Start,
                    token.BodyRelativeSpan.Length))
            .OrderBy(static token => token.Span.Start)
            .ThenBy(static token => token.Span.Length)
            .ToImmutableArray();

    private static string? GetKindName(
        IMacroTokenKindProvider? provider,
        SyntaxToken token)
    {
        if (provider is not null)
        {
            try
            {
                var name = provider.GetTokenKindName(token.RawKind);
                if (!string.IsNullOrWhiteSpace(name))
                    return name;
            }
            catch
            {
                // Optional tooling metadata must not invalidate the token snapshot.
            }
        }

        return Enum.IsDefined(typeof(SyntaxKind), token.RawKind)
            ? ((SyntaxKind)token.RawKind).ToString()
            : null;
    }

    private static MacroTokenClassification GetClassification(
        IMacroTokenClassifier? classifier,
        TokenTreeMacroContext context,
        SyntaxToken token)
    {
        var classification = MacroTokenClassification.Default;
        if (classifier is not null)
        {
            try
            {
                classification = classifier.ClassifyToken(context, token);
            }
            catch
            {
                // Fall through to compiler-owned keyword classification.
            }
        }

        if (!Enum.IsDefined(classification))
            classification = MacroTokenClassification.Default;

        return classification == MacroTokenClassification.Default
            ? context.GetKeywordClassification(token)
            : classification;
    }

    private static ISymbol? GetSymbol(
        IMacroTokenSymbolProvider? provider,
        TokenTreeMacroContext context,
        SyntaxToken token)
    {
        if (provider is null)
            return null;

        try
        {
            return provider.GetTokenSymbol(context, token);
        }
        catch
        {
            // Optional semantic metadata must not invalidate the token snapshot.
            return null;
        }
    }
}
