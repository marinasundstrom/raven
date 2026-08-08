using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroFragmentRegionService
{
    public static ImmutableArray<MacroFragmentRegion> GetFragmentRegions(
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
            loaded.Macro is not ITokenTreeExpressionMacro tokenTreeMacro ||
            loaded.Macro is not IMacroFragmentProvider fragmentProvider)
        {
            return ImmutableArray<MacroFragmentRegion>.Empty;
        }

        try
        {
            var context = new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                expression,
                tokenTreeMacro,
                cancellationToken);
            var regions = fragmentProvider.GetFragmentRegions(context);
            cancellationToken.ThrowIfCancellationRequested();
            return regions.IsDefaultOrEmpty
                ? ImmutableArray<MacroFragmentRegion>.Empty
                : regions.Where(static region => region is not null).ToImmutableArray();
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch
        {
            // Tooling queries must remain available when an optional macro capability fails.
            return ImmutableArray<MacroFragmentRegion>.Empty;
        }
    }
}
