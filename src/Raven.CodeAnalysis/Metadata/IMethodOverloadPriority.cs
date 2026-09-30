namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Optional provider-specific overload priority unavailable through ordinary
/// semantic attributes. Providers own encoding and inherited-declaration rules;
/// the compiler owns applicability, grouping and priority comparison.
/// </summary>
internal interface IMethodOverloadPriority
{
    bool TryGetOverloadResolutionPriority(out int priority);
}
