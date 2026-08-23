namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes whether a macro carrier accepts a trailing token-tree body.
/// </summary>
public enum MacroBodyRequirement
{
    /// <summary>
    /// Infers the compatibility behavior from the macro's typed inputs.
    /// </summary>
    Default = 0,

    None = 1,
    Optional = 2,
    Required = 3,
}
