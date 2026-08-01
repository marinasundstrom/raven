using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Combines a type's declared nullable annotation with the contextual flow
/// state of a particular value. The two dimensions are intentionally independent.
/// </summary>
[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public NullabilityInfo(NullableAnnotation annotation, NullableFlowState flowState)
    {
        Annotation = annotation;
        FlowState = flowState;
    }

    /// <summary>
    /// Gets whether null is part of the declared type's value domain.
    /// </summary>
    public NullableAnnotation Annotation { get; }

    /// <summary>
    /// Gets what flow analysis knows about the value at this program point.
    /// </summary>
    public NullableFlowState FlowState { get; }

    public bool Equals(NullabilityInfo other)
    {
        return Annotation == other.Annotation && FlowState == other.FlowState;
    }

    public override bool Equals(object? obj)
    {
        return obj is NullabilityInfo other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine((int)Annotation, (int)FlowState);
    }

    private string GetDebuggerDisplay()
    {
        return $"{Annotation} ({FlowState})";
    }
}
