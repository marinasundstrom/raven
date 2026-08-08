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

    /// <summary>
    /// Finds the most specific Raven fragment region at an authored source position.
    /// </summary>
    public MacroFragmentRegion? FindFragmentRegion(int position)
    {
        if (position < BodySpan.Start || position > BodySpan.End)
            return null;

        MacroFragmentRegion? best = null;
        foreach (var region in FragmentRegions)
        {
            var containsPosition = region.Span.Length == 0
                ? position == region.Span.Start
                : position >= region.Span.Start && position <= region.Span.End;
            if (!containsPosition)
                continue;

            if (best is null || region.Span.Length < best.Span.Length)
                best = region;
        }

        return best;
    }
}
