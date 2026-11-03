using System.Linq;

namespace Raven.CodeAnalysis.Targets;

internal abstract partial class CliRuntimeContract
{
    protected abstract string ArraySliceMethodName { get; }

    // The .NET path retains its existing runtime-helper signature policy. Native
    // contracts additionally require their target-specific view result shape.
    protected virtual bool IsArraySliceResultType(TypeSymbol resultType, TypeSymbol elementType) => true;

    // Resolve runtime representation only. Range syntax, conversions, rank checks
    // and source diagnostics remain with the binder. Do not cache snapshot symbols.
    internal MethodSymbol? ResolveArraySliceMethod(
        Compilation compilation, ArrayTypeSymbol arrayType, TypeSymbol rangeType)
    {
        var runtimeHelpers = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeHelpers");
        if (runtimeHelpers is null || runtimeHelpers.TypeKind == TypeKind.Error || rangeType.TypeKind == TypeKind.Error)
            return null;

        foreach (var candidate in runtimeHelpers.GetMembers(ArraySliceMethodName).OfType<MethodSymbol>())
        {
            if (!candidate.IsStatic || !candidate.IsGenericMethod || candidate.TypeParameters.Length != 1 || candidate.Parameters.Length != 2)
                continue;

            if (candidate.Parameters[0].Type is not ArrayTypeSymbol parameterArray || parameterArray.Rank != 1)
                continue;

            if (!SymbolEqualityComparer.Default.Equals(candidate.Parameters[1].Type, rangeType))
                continue;

            var constructed = candidate.Construct(arrayType.ElementType);
            if (!IsArraySliceResultType(constructed.ReturnType, arrayType.ElementType))
                continue;
            return constructed;
        }

        return null;
    }
}
