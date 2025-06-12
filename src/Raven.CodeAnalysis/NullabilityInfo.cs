namespace Raven.CodeAnalysis;

[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public NullableAnnotation Annotation { get; }

    public NullableFlowState FlowState { get; }

    public bool Equals(NullabilityInfo other)
    {
        throw new NotImplementedException();
    }
}