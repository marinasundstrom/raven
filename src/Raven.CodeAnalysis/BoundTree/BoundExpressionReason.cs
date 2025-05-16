namespace Raven.CodeAnalysis;

public enum BoundExpressionReason
{
    None,                   // Successfully bound, no issues.
    NotFound,               // Symbol or member could not be found.
    OverloadResolutionFailed, // Overload candidates found, but none applicable.
    Ambiguous,              // Multiple matches found, ambiguity unresolved.
    Inaccessible,           // Symbol exists but is inaccessible (private/internal/etc).
    WrongArity,             // Wrong number of type arguments.
    TypeMismatch,           // Expression type incompatible with expected type.
    MissingType,            // Type information could not be resolved.
    ConstantExpected,       // Expression is not a constant where one was required.
    UnsupportedOperation,   // Operation is not valid for this expression.
    OtherError              // Generic catch-all for other binding failures.
}