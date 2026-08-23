namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies the source carrier shapes accepted by a freestanding macro.
/// </summary>
[Flags]
public enum MacroCarrierKinds
{
    /// <summary>
    /// Uses the compatibility carrier inferred from the macro's typed inputs.
    /// </summary>
    Default = 0,

    Parenthesized = 1 << 0,
    ExpressionHeader = 1 << 1,
    TokenTree = 1 << 2,
    Declaration = 1 << 3,
}
