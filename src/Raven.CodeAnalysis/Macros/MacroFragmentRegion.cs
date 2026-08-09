using System.Collections.Immutable;

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
        TextSpan span,
        ImmutableArray<MacroFragmentLocal> locals,
        ITypeSymbol? targetType)
    {
        Kind = kind;
        BodyRelativeSpan = bodyRelativeSpan;
        Span = span;
        Locals = locals.IsDefault ? ImmutableArray<MacroFragmentLocal>.Empty : locals;
        TargetType = targetType;
    }

    /// <summary>Gets the Raven syntax category expected in this region.</summary>
    public MacroFragmentKind Kind { get; }

    /// <summary>Gets the region relative to the start of the macro body.</summary>
    public TextSpan BodyRelativeSpan { get; }

    /// <summary>Gets the region in the containing Raven source text.</summary>
    public TextSpan Span { get; }

    /// <summary>Gets the macro-introduced locals visible inside this region.</summary>
    public ImmutableArray<MacroFragmentLocal> Locals { get; }

    /// <summary>Gets the optional target type used to bind an expression region.</summary>
    public ITypeSymbol? TargetType { get; }
}
