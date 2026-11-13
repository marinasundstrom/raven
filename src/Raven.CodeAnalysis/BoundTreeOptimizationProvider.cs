namespace Raven.CodeAnalysis;

// Compiler-owned context for the shared bound-tree optimization pipeline.
// Source assemblies supply policy and selected-target type identities.
internal interface BoundTreeOptimizationProvider
{
    bool IsReleaseOptimizationEnabled { get; }

    OperatorTypeProvider OperatorTypes { get; }
}
