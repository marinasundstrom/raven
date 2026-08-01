namespace Raven.CodeAnalysis;

/// <summary>
/// Describes what nullable flow analysis knows about a value at a particular
/// program point. It does not change the value's declared type annotation.
/// </summary>
public enum NullableFlowState
{
    /// <summary>
    /// No value or flow information is available.
    /// </summary>
    None,

    /// <summary>
    /// The value is known not to be null at this program point.
    /// </summary>
    NotNull,

    /// <summary>
    /// The value may be null at this program point.
    /// </summary>
    MaybeNull
}
