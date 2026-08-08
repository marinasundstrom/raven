using System.Collections.Immutable;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides the compiler-owned token-and-fragment view of one token-tree macro body.
/// </summary>
public sealed class MacroInputSnapshot
{
    internal MacroInputSnapshot(
        TextSpan bodySpan,
        ImmutableArray<MacroTokenInfo> tokens,
        ImmutableArray<MacroFragmentRegion> fragmentRegions)
    {
        BodySpan = bodySpan;
        Tokens = tokens;
        FragmentRegions = fragmentRegions;
    }

    public TextSpan BodySpan { get; }

    public ImmutableArray<MacroTokenInfo> Tokens { get; }

    public ImmutableArray<MacroFragmentRegion> FragmentRegions { get; }
}
