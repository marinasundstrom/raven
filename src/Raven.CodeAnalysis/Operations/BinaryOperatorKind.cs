namespace Raven.CodeAnalysis.Operations;

/// <summary>The resolved binary operator, independent of source spelling and lifting/overflow flags.</summary>
public enum BinaryOperatorKind
{
    /// <summary>No ordinary binary operator (for example, coalescing).</summary>
    None,
    /// <summary>Add operation.</summary>
    Add,
    /// <summary>Subtract operation.</summary>
    Subtract,
    /// <summary>Multiply operation.</summary>
    Multiply,
    /// <summary>Divide operation.</summary>
    Divide,
    /// <summary>Equals operation.</summary>
    Equals,
    /// <summary>NotEquals operation.</summary>
    NotEquals,
    /// <summary>GreaterThan operation.</summary>
    GreaterThan,
    /// <summary>LessThan operation.</summary>
    LessThan,
    /// <summary>GreaterThanOrEqual operation.</summary>
    GreaterThanOrEqual,
    /// <summary>LessThanOrEqual operation.</summary>
    LessThanOrEqual,
    /// <summary>Remainder operation.</summary>
    Remainder,
    /// <summary>And operation.</summary>
    And,
    /// <summary>Or operation.</summary>
    Or,
    /// <summary>ExclusiveOr operation.</summary>
    ExclusiveOr,
    /// <summary>ConditionalAnd operation.</summary>
    ConditionalAnd,
    /// <summary>ConditionalOr operation.</summary>
    ConditionalOr,
    /// <summary>Concatenate operation.</summary>
    Concatenate,
    /// <summary>LeftShift operation.</summary>
    LeftShift,
    /// <summary>RightShift operation.</summary>
    RightShift,
}
