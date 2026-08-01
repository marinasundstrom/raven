namespace Raven.CodeAnalysis;

/// <summary>
/// Describes whether null is part of a type's declared value domain.
/// </summary>
public enum NullableAnnotation
{
    /// <summary>
    /// No declared type is available, or nullable annotation is not applicable.
    /// This is not a valid annotation for <c>WithNullableAnnotation</c>.
    /// </summary>
    None,

    /// <summary>
    /// Null is not part of the declared value domain.
    /// </summary>
    NotAnnotated,

    /// <summary>
    /// Null is part of the declared value domain.
    /// </summary>
    Annotated
}
