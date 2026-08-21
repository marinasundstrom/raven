using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroEmbeddedLanguageProjectionService
{
    public static MacroEmbeddedLanguageProjection? GetProjection(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!FreestandingMacroInvocation.TryCreate(syntax, out var invocation) ||
            invocation.TokenTree is null ||
            !invocation.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                syntax,
                name,
                out var loaded,
                out _) ||
            !loaded.Descriptor.HasTokenBody ||
            loaded.Macro is not IMacroEmbeddedLanguageProvider provider)
        {
            return null;
        }

        try
        {
            var context = new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                invocation,
                loaded.Macro,
                cancellationToken);
            var projection = provider.GetEmbeddedLanguageProjection(context);
            cancellationToken.ThrowIfCancellationRequested();

            if (projection is null ||
                string.IsNullOrWhiteSpace(projection.LanguageId) ||
                projection.Span != context.BodySpan ||
                projection.Text.Length != context.BodySpan.Length)
            {
                return null;
            }

            return projection;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch
        {
            // Optional macro tooling must not break ordinary editor requests.
            return null;
        }
    }
}
