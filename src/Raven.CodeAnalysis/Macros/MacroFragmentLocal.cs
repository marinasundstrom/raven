using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes a local value introduced by a macro and visible inside one Raven fragment.
/// </summary>
public sealed class MacroFragmentLocal
{
    internal MacroFragmentLocal(
        string name,
        ITypeSymbol type,
        TextSpan? bodyRelativeDeclarationSpan,
        TextSpan? declarationSpan)
    {
        Name = name;
        Type = type;
        BodyRelativeDeclarationSpan = bodyRelativeDeclarationSpan;
        DeclarationSpan = declarationSpan;
    }

    /// <summary>Gets the local's Raven name.</summary>
    public string Name { get; }

    /// <summary>Gets the type visible to ordinary Raven tooling in the fragment.</summary>
    public ITypeSymbol Type { get; }

    /// <summary>
    /// Gets the optional body-relative span that declares this local in the macro DSL.
    /// </summary>
    public TextSpan? BodyRelativeDeclarationSpan { get; }

    /// <summary>
    /// Gets the optional authored span that declares this local in the invocation.
    /// </summary>
    public TextSpan? DeclarationSpan { get; }
}
