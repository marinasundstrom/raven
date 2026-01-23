using System;

namespace Raven.CodeAnalysis;

[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public NullabilityInfo(NullableAnnotation annotation, NullableFlowState flowState)
    {
        Annotation = annotation;
        FlowState = flowState;
    }

    public NullableAnnotation Annotation { get; }

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
