namespace Raven.CodeAnalysis;

/// <summary>
/// Describes whether a script or interactive submission is ready to compile.
/// </summary>
public enum SubmissionCompleteness
{
    /// <summary>
    /// The submission is syntactically valid and complete.
    /// </summary>
    Complete = 0,

    /// <summary>
    /// The submission ends while the parser is still expecting more input.
    /// </summary>
    Incomplete = 1,

    /// <summary>
    /// The submission is complete but contains syntax errors.
    /// </summary>
    Invalid = 2
}
