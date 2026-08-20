using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies the single-node grammar positions in which a freestanding macro
/// can appear.
/// </summary>
[Flags]
public enum MacroInvocationTargets
{
    None = 0,
    Expression = 1 << 0,
    Statement = 1 << 1,
    NamespaceMember = 1 << 2,
    TypeMember = 1 << 3,
    Type = 1 << 4,
    Pattern = 1 << 5,

    /// <summary>
    /// Includes every supported single-node freestanding grammar position.
    /// </summary>
    AllSingleNode = Expression | Statement | NamespaceMember | TypeMember | Type | Pattern,
}
