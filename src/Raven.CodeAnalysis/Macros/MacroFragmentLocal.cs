namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes a local value introduced by a macro and visible inside one Raven fragment.
/// </summary>
public sealed class MacroFragmentLocal
{
    internal MacroFragmentLocal(string name, ITypeSymbol type)
    {
        Name = name;
        Type = type;
    }

    /// <summary>Gets the local's Raven name.</summary>
    public string Name { get; }

    /// <summary>Gets the type visible to ordinary Raven tooling in the fragment.</summary>
    public ITypeSymbol Type { get; }
}
