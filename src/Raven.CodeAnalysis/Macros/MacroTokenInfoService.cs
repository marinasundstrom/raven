using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

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
            var classifier = loaded.Macro as IMacroTokenClassifier;
            var stream = context.CreateTokenStream();
            var builder = ImmutableArray.CreateBuilder<MacroTokenInfo>();

            while (!stream.IsEndOfFile)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var token = stream.ReadToken();
                var classification = classifier?.ClassifyToken(context, token)
                    ?? MacroTokenClassification.Default;
                if (classification == MacroTokenClassification.Default)
                    classification = context.GetKeywordClassification(token);

                builder.Add(context.CreateTokenInfo(token, classification));
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
}
