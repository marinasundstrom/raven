using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundLambdaExpression : BoundExpression
{
    public IEnumerable<IParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public ITypeSymbol ReturnType { get; }
    public ITypeSymbol DelegateType { get; }
    public IEnumerable<ISymbol> CapturedVariables { get; }
    public ImmutableArray<INamedTypeSymbol> CandidateDelegates { get; }

    public BoundLambdaExpression(
        IEnumerable<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        BoundExpression body,
        ISymbol symbol,
        ITypeSymbol delegateType,
        IEnumerable<ISymbol> capturedVariables,
        ImmutableArray<INamedTypeSymbol> candidateDelegates)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
        CapturedVariables = capturedVariables;
        CandidateDelegates = candidateDelegates;
    }

    public bool IsCompatibleWithDelegate(INamedTypeSymbol delegateType, Compilation compilation)
    {
        if (delegateType.TypeKind != TypeKind.Delegate)
            return false;

        var targetInvoke = delegateType.GetDelegateInvokeMethod();
        if (targetInvoke is null)
            return false;

        var lambdaParameters = Parameters.ToImmutableArray();

        if (CandidateDelegates.IsDefaultOrEmpty)
            return targetInvoke.Parameters.Length == lambdaParameters.Length;

        foreach (var candidate in CandidateDelegates)
        {
            if (candidate.TypeKind != TypeKind.Delegate)
                continue;

            if (SymbolEqualityComparer.Default.Equals(candidate, delegateType))
                return true;

            var candidateInvoke = candidate.GetDelegateInvokeMethod();
            if (candidateInvoke is null)
                continue;

            if (!HaveCompatibleSignature(candidateInvoke, targetInvoke, lambdaParameters, ReturnType, compilation))
                continue;

            return true;
        }

        return false;

        static bool HaveCompatibleSignature(
            IMethodSymbol source,
            IMethodSymbol target,
            ImmutableArray<IParameterSymbol> lambdaParameters,
            ITypeSymbol lambdaReturnType,
            Compilation compilation)
        {
            var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

            if (source.ContainingType is INamedTypeSymbol sourceDelegate &&
                target.ContainingType is INamedTypeSymbol targetDelegate)
            {
                if (!TryAddTypeMappings(sourceDelegate, targetDelegate, substitutions))
                    return false;
            }

            if (source.Parameters.Length != target.Parameters.Length)
                return false;

            for (int i = 0; i < source.Parameters.Length; i++)
            {
                var sourceParameter = source.Parameters[i];
                var targetParameter = target.Parameters[i];

                if (sourceParameter.RefKind != targetParameter.RefKind)
                    return false;

                if (!TryAddTypeMappings(sourceParameter.Type, targetParameter.Type, substitutions))
                    return false;

                if (i < lambdaParameters.Length)
                {
                    var lambdaParameter = lambdaParameters[i];
                    if (lambdaParameter.Type is ITypeSymbol lambdaType && lambdaType.TypeKind != TypeKind.Error)
                    {
                        if (!TryAddTypeMappings(sourceParameter.Type, lambdaType, substitutions))
                            return false;

                        if (!TryAddTypeMappings(targetParameter.Type, lambdaType, substitutions))
                            return false;
                    }
                }

                var substitutedSource = SubstituteType(sourceParameter.Type, substitutions, compilation);
                var substitutedTarget = SubstituteType(targetParameter.Type, substitutions, compilation);

                if (SymbolEqualityComparer.Default.Equals(substitutedSource, substitutedTarget))
                    continue;

                var conversion = compilation.ClassifyConversion(substitutedSource, substitutedTarget);
                if (!conversion.Exists || !conversion.IsImplicit)
                    return false;
            }

            if (!TryAddTypeMappings(source.ReturnType, target.ReturnType, substitutions))
                return false;

            if (lambdaReturnType is not null && lambdaReturnType.TypeKind != TypeKind.Error)
            {
                if (!TryAddTypeMappings(source.ReturnType, lambdaReturnType, substitutions))
                    return false;

                if (!TryAddTypeMappings(target.ReturnType, lambdaReturnType, substitutions))
                    return false;
            }

            var substitutedReturn = SubstituteType(source.ReturnType, substitutions, compilation);
            var substitutedTargetReturn = SubstituteType(target.ReturnType, substitutions, compilation);

            if (SymbolEqualityComparer.Default.Equals(substitutedReturn, substitutedTargetReturn))
                return true;

            var returnConversion = compilation.ClassifyConversion(substitutedReturn, substitutedTargetReturn);
            return returnConversion.Exists && returnConversion.IsImplicit;

            static bool TryAddTypeMappings(
                ITypeSymbol sourceType,
                ITypeSymbol targetType,
                Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
            {
                if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                    return true;

                if (sourceType is ITypeParameterSymbol typeParameter)
                {
                    if (substitutions.TryGetValue(typeParameter, out var existing))
                        return SymbolEqualityComparer.Default.Equals(existing, targetType);

                    substitutions[typeParameter] = targetType;
                    return true;
                }

                switch (sourceType)
                {
                    case INamedTypeSymbol sourceNamed when targetType is INamedTypeSymbol targetNamed:
                        {
                            var sourceDefinition = sourceNamed.ConstructedFrom ?? sourceNamed;
                            var targetDefinition = targetNamed.ConstructedFrom ?? targetNamed;

                            if (!SymbolEqualityComparer.Default.Equals(sourceDefinition, targetDefinition))
                                return false;

                            var sourceArgs = sourceNamed.TypeArguments;
                            var targetArgs = targetNamed.TypeArguments;

                            if (sourceArgs.IsDefaultOrEmpty || targetArgs.IsDefaultOrEmpty)
                                return true;

                            if (sourceArgs.Length != targetArgs.Length)
                                return false;

                            for (int i = 0; i < sourceArgs.Length; i++)
                            {
                                if (!TryAddTypeMappings(sourceArgs[i], targetArgs[i], substitutions))
                                    return false;
                            }

                            return true;
                        }

                    case IArrayTypeSymbol sourceArray when targetType is IArrayTypeSymbol targetArray:
                        if (sourceArray.Rank != targetArray.Rank)
                            return true;

                        return TryAddTypeMappings(sourceArray.ElementType, targetArray.ElementType, substitutions);

                    case NullableTypeSymbol sourceNullable when targetType is NullableTypeSymbol targetNullable:
                        return TryAddTypeMappings(sourceNullable.UnderlyingType, targetNullable.UnderlyingType, substitutions);

                    case ByRefTypeSymbol sourceByRef when targetType is ByRefTypeSymbol targetByRef:
                        return TryAddTypeMappings(sourceByRef.ElementType, targetByRef.ElementType, substitutions);

                    case IAddressTypeSymbol sourceAddress when targetType is IAddressTypeSymbol targetAddress:
                        return TryAddTypeMappings(sourceAddress.ReferencedType, targetAddress.ReferencedType, substitutions);
                }

                return true;
            }

            static ITypeSymbol SubstituteType(
                ITypeSymbol sourceType,
                Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
                Compilation compilation)
            {
                if (sourceType is ITypeParameterSymbol typeParameter &&
                    substitutions.TryGetValue(typeParameter, out var mapped))
                {
                    return mapped;
                }

                switch (sourceType)
                {
                    case INamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty && named.TypeArguments.Length > 0:
                        {
                            var typeArguments = named.TypeArguments;
                            var substitutedArgs = new ITypeSymbol[typeArguments.Length];
                            bool changed = false;

                            for (int i = 0; i < typeArguments.Length; i++)
                            {
                                var substituted = SubstituteType(typeArguments[i], substitutions, compilation);
                                substitutedArgs[i] = substituted;
                                changed |= !SymbolEqualityComparer.Default.Equals(substituted, typeArguments[i]);
                            }

                            if (changed && named.ConstructedFrom is INamedTypeSymbol definition)
                            {
                                return (INamedTypeSymbol)definition.Construct(substitutedArgs);
                            }

                            return named;
                        }

                    case IArrayTypeSymbol arrayType:
                        {
                            var substitutedElement = SubstituteType(arrayType.ElementType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, arrayType.ElementType))
                            {
                                return compilation.CreateArrayTypeSymbol(substitutedElement, arrayType.Rank);
                            }

                            return arrayType;
                        }

                    case NullableTypeSymbol nullableType:
                        {
                            var substitutedUnderlying = SubstituteType(nullableType.UnderlyingType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedUnderlying, nullableType.UnderlyingType))
                            {
                                return new NullableTypeSymbol(substitutedUnderlying, null, null, null, []);
                            }

                            return nullableType;
                        }

                    case ByRefTypeSymbol byRefType:
                        {
                            var substitutedElement = SubstituteType(byRefType.ElementType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, byRefType.ElementType))
                            {
                                return new ByRefTypeSymbol(substitutedElement);
                            }

                            return byRefType;
                        }

                    case IAddressTypeSymbol addressType:
                        {
                            var substitutedElement = SubstituteType(addressType.ReferencedType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, addressType.ReferencedType))
                            {
                                return new AddressTypeSymbol(substitutedElement);
                            }

                            return (ITypeSymbol)addressType;
                        }
                }

                return sourceType;
            }
        }
    }
}
