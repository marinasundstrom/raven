using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes a region of a token-tree macro body that contains ordinary Raven syntax.
/// </summary>
public sealed class MacroFragmentRegion
{
    internal MacroFragmentRegion(
        MacroFragmentKind kind,
        TextSpan bodyRelativeSpan,
        TextSpan span)
    {
        Kind = kind;
        BodyRelativeSpan = bodyRelativeSpan;
        Span = span;
    }

    /// <summary>Gets the Raven syntax category expected in this region.</summary>
    public MacroFragmentKind Kind { get; }

    /// <summary>Gets the region relative to the start of the macro body.</summary>
    public TextSpan BodyRelativeSpan { get; }

    /// <summary>Gets the region in the containing Raven source text.</summary>
    public TextSpan Span { get; }
}
