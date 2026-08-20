namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies how a macro is applied to authored Raven syntax.
/// </summary>
public enum MacroApplicationKind
{
    /// <summary>
    /// The procedural macro appears independently at one of its permitted
    /// grammar positions.
    /// </summary>
    Freestanding = 0,

    /// <summary>
    /// The procedural macro is attached to an existing syntax declaration in
    /// an attribute-like position.
    /// </summary>
    Attached = 1,
}
