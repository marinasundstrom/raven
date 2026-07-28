namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies how a macro function parameter is supplied.
/// </summary>
public enum MacroParameterRole
{
    /// <summary>
    /// The parameter does not belong to a macro function.
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
}
