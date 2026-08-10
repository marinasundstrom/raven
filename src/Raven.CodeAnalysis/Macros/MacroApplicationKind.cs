namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies how a macro is applied to authored Raven syntax.
/// </summary>
public enum MacroApplicationKind
{
    /// <summary>
    /// The macro is invoked from a compiler-owned grammar carrier.
    /// </summary>
    Invocable = 0,

    /// <summary>
    /// The macro is attached to an existing syntax declaration.
    /// </summary>
    Attached = 1,
}
