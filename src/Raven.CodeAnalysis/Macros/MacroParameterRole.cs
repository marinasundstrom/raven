namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies how a macro declaration parameter is supplied.
/// </summary>
public enum MacroParameterRole
{
    /// <summary>
    /// The parameter does not belong to a macro declaration.
    /// </summary>
    None = 0,

    /// <summary>
    /// The caller supplies the parameter through the macro invocation's
    /// argument list.
    /// </summary>
    Value = 1,

    /// <summary>
    /// The compiler projects an invocation argument as source-backed Raven
    /// syntax. The parameter type identifies the required syntax category.
    /// </summary>
    SyntaxInput = 2,

    /// <summary>
    /// The compiler supplies the parameter from the invocation's raw
    /// token-tree body.
    /// </summary>
    TokenBody = 3,

    /// <summary>
    /// The compiler supplies a recognized macro context object. The parameter
    /// type identifies the concrete context capabilities.
    /// </summary>
    Context = 4,

    /// <summary>
    /// The compiler supplies the syntax node to which an attached macro is
    /// applied.
    /// </summary>
    AttachedTarget = 5,

    /// <summary>
    /// The compiler supplies the complete source-backed declaration-shaped
    /// freestanding macro carrier.
    /// </summary>
    DeclarationInput = 6,
}
