using System;
using System.Linq;

namespace Raven.CodeAnalysis;

[System.Diagnostics.DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct NullabilityInfo : IEquatable<NullabilityInfo>
{
    public static NullabilityInfo FromType(ITypeSymbol? typeSymbol)
    {
        if (typeSymbol is null)
            return new NullabilityInfo(NullableAnnotation.None, NullableFlowState.None);

        if (typeSymbol.TypeKind == TypeKind.Null)
            return new NullabilityInfo(NullableAnnotation.Annotated, NullableFlowState.MaybeNull);

        if (typeSymbol.IsNullable)
            return new NullabilityInfo(NullableAnnotation.Annotated, NullableFlowState.MaybeNull);

        if (typeSymbol is ITypeUnionSymbol union && union.Types.Any(static type => type.TypeKind == TypeKind.Null))
            return new NullabilityInfo(NullableAnnotation.Annotated, NullableFlowState.MaybeNull);

        return new NullabilityInfo(NullableAnnotation.NotAnnotated, NullableFlowState.NotNull);
    }

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
