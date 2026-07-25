using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal enum RefSafetyViolationKind
{
    StackAllocationEscape,
    LocalReferenceEscape,
    ScopedValueEscape,
}

internal readonly record struct RefSafetyViolation(
    RefSafetyViolationKind Kind,
    BoundExpression Expression,
    ISymbol? Origin = null);

internal sealed class RefSafetyAnalysisResult
{
    public RefSafetyAnalysisResult(ImmutableArray<RefSafetyViolation> violations)
    {
        Violations = violations;
    }

    public ImmutableArray<RefSafetyViolation> Violations { get; }
}
