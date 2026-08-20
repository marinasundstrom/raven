using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes an inferred type annotation that can be presented in authored Raven syntax inside a macro fragment.
/// </summary>
public sealed class MacroFragmentInferredTypeAnnotation
{
    internal MacroFragmentInferredTypeAnnotation(TextSpan span, ITypeSymbol type)
    {
        Span = span;
        Type = type;
    }

    /// <summary>Gets the authored identifier span after which the type annotation is inserted.</summary>
    public TextSpan Span { get; }

    /// <summary>Gets the inferred type.</summary>
    public ITypeSymbol Type { get; }
}
