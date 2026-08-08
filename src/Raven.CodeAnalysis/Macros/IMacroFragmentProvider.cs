using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Optionally identifies ordinary Raven fragments embedded in a token-tree macro body.
/// </summary>
public interface IMacroFragmentProvider
{
    ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context);
}
