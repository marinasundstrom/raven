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
        FreestandingMacroDeclarationSyntax declaration,
        CancellationToken cancellationToken)
        => GetFragmentRegions(semanticModel, declaration, declaration, cancellationToken);

    public static ImmutableArray<MacroFragmentRegion> GetFragmentRegions(
        SemanticModel semanticModel,
        SyntaxNode syntax,
        SyntaxNode resolutionContext,
        CancellationToken cancellationToken)
    {
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
            !loaded.Descriptor.HasTokenBody)
        {
            return ImmutableArray<MacroFragmentRegion>.Empty;
        }

        try
        {
            var context = new TokenTreeMacroContext(
                semanticModel.Compilation,
                semanticModel,
                invocation,
                loaded.Macro,
                cancellationToken);
            ImmutableArray<MacroFragmentRegion> regions;
            if (loaded.Macro is IMacroFragmentProvider fragmentProvider)
            {
                regions = fragmentProvider.GetFragmentRegions(context);
            }
            else if (loaded.Macro is IMacroExpansionMetadataProvider)
            {
                regions = semanticModel.GetFreestandingMacroExpansion(syntax, cancellationToken)?.FragmentRegions ??
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
