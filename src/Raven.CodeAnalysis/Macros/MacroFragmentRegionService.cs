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
            loaded.Macro is not ITokenTreeExpressionMacro tokenTreeMacro)
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
            var regions = loaded.Macro is IMacroFragmentProvider fragmentProvider
                ? fragmentProvider.GetFragmentRegions(context)
                : semanticModel.GetMacroExpansion(expression, cancellationToken)?.FragmentRegions ??
                    ImmutableArray<MacroFragmentRegion>.Empty;
            cancellationToken.ThrowIfCancellationRequested();
            return regions.IsDefaultOrEmpty
                ? ImmutableArray<MacroFragmentRegion>.Empty
                : regions
                    .Where(static region => region is not null)
                    .OrderBy(static region => region.Span.Start)
                    .ThenBy(static region => region.Span.Length)
                    .ToImmutableArray();
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
