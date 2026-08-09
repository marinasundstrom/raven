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
    /// The compiler projects an invocation argument as Raven expression
    /// syntax.
    /// </summary>
    ExpressionSyntax = 2,

    /// <summary>
    /// The compiler supplies the parameter from the invocation's raw
    /// token-tree body.
    /// </summary>
    TokenStream = 3,

    /// <summary>
    /// The compiler supplies the complete token-tree expansion context.
    /// </summary>
    Context = 4,

    /// <summary>
    /// The compiler supplies the complete argument-style freestanding expansion
    /// context without requiring a token-tree body.
    /// </summary>
    FreestandingContext = 5,

    /// <summary>
    /// The compiler supplies the complete attached expansion context.
    /// </summary>
    AttachedContext = 6,
}
