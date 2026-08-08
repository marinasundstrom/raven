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
        => GetFragmentRegions(semanticModel, expression, expression, cancellationToken);

    public static ImmutableArray<MacroFragmentRegion> GetFragmentRegions(
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax expression,
        SyntaxNode resolutionContext,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (expression.TokenTree is null ||
            !expression.TryGetMacroName(out var name) ||
            !semanticModel.Compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                semanticModel.Compilation,
                resolutionContext,
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
            ImmutableArray<MacroFragmentRegion> regions;
            if (loaded.Macro is IMacroFragmentProvider fragmentProvider)
            {
                regions = fragmentProvider.GetFragmentRegions(context);
            }
            else if (loaded.Macro is IMacroExpansionMetadataProvider)
            {
                regions = semanticModel.GetMacroExpansion(expression, cancellationToken)?.FragmentRegions ??
                    ImmutableArray<MacroFragmentRegion>.Empty;
            }
            else
            {
                regions = ImmutableArray<MacroFragmentRegion>.Empty;
            }
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
