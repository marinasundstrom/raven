namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies the Raven syntax expected inside a region of a token-tree macro body.
/// </summary>
public enum MacroFragmentKind
{
    Expression = 0,
    Statement = 1,
    Type = 2,
    Pattern = 3,
    MemberDeclaration = 4,
    /// <summary>
    /// A sequence of Raven statements sharing one lexical block scope.
    /// </summary>
    Block = 5,
}
