using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides an alternate unqualified invocation name for a macro.
/// </summary>
/// <remarks>
/// The alias enters scope through the same namespace imports as the macro's
/// declared name. It is not globally available merely because the macro
/// assembly is referenced.
/// </remarks>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = false, Inherited = false)]
public sealed class MacroAliasAttribute : Attribute
{
    public MacroAliasAttribute(string alias)
    {
        if (string.IsNullOrWhiteSpace(alias))
            throw new ArgumentException("A macro alias is required.", nameof(alias));

        Alias = alias;
    }

    public string Alias { get; }
}
