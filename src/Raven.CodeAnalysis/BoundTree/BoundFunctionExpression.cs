using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundFunctionExpression : BoundExpression
{
    public IEnumerable<ParameterSymbol> Parameters { get; }
    public BoundExpression Body { get; }
    public TypeSymbol ReturnType { get; }
    public TypeSymbol DelegateType { get; }
    public IEnumerable<Symbol> CapturedVariables { get; }
    public ImmutableArray<NamedTypeSymbol> CandidateDelegates { get; }

    public BoundFunctionExpression(
        IEnumerable<ParameterSymbol> parameters,
        TypeSymbol returnType,
        BoundExpression body,
        Symbol symbol,
        TypeSymbol delegateType,
        IEnumerable<Symbol> capturedVariables,
        ImmutableArray<NamedTypeSymbol> candidateDelegates)
        : base(delegateType, symbol, BoundExpressionReason.None)
    {
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
        DelegateType = delegateType;
        CapturedVariables = capturedVariables;
        CandidateDelegates = candidateDelegates;
    }

    public bool IsCompatibleWithDelegate(NamedTypeSymbol delegateType, Compilation compilation)
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
            MethodSymbol source,
            MethodSymbol target,
            ImmutableArray<ParameterSymbol> lambdaParameters,
            TypeSymbol lambdaReturnType,
            Compilation compilation)
        {
            var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

            if (source.ContainingType is NamedTypeSymbol sourceDelegate &&
                target.ContainingType is NamedTypeSymbol targetDelegate)
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
                    if (lambdaParameter.Type is TypeSymbol lambdaType && lambdaType.TypeKind != TypeKind.Error)
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
                TypeSymbol sourceType,
                TypeSymbol targetType,
                Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
            {
                if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                    return true;

                if (sourceType is TypeParameterSymbol typeParameter)
                {
                    if (substitutions.TryGetValue(typeParameter, out var existing))
                        return SymbolEqualityComparer.Default.Equals(existing, targetType);

                    substitutions[typeParameter] = targetType;
                    return true;
                }

                switch (sourceType)
                {
                    case NamedTypeSymbol sourceNamed when targetType is NamedTypeSymbol targetNamed:
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

                    case ArrayTypeSymbol sourceArray when targetType is ArrayTypeSymbol targetArray:
                        if (sourceArray.Rank != targetArray.Rank)
                            return true;

                        return TryAddTypeMappings(sourceArray.ElementType, targetArray.ElementType, substitutions);

                    case NullableTypeSymbol sourceNullable when targetType is NullableTypeSymbol targetNullable:
                        return TryAddTypeMappings(sourceNullable.UnderlyingType, targetNullable.UnderlyingType, substitutions);

                    case RefTypeSymbol sourceByRef when targetType is RefTypeSymbol targetByRef:
                        return TryAddTypeMappings(sourceByRef.ElementType, targetByRef.ElementType, substitutions);

                    case AddressTypeSymbol sourceAddress when targetType is AddressTypeSymbol targetAddress:
                        return TryAddTypeMappings(sourceAddress.ReferencedType, targetAddress.ReferencedType, substitutions);
                }

                return true;
            }

            static TypeSymbol SubstituteType(
                TypeSymbol sourceType,
                Dictionary<TypeParameterSymbol, TypeSymbol> substitutions,
                Compilation compilation)
            {
                if (sourceType is TypeParameterSymbol typeParameter &&
                    substitutions.TryGetValue(typeParameter, out var mapped))
                {
                    return mapped;
                }

                switch (sourceType)
                {
                    case NamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty && named.TypeArguments.Length > 0:
                        {
                            var typeArguments = named.TypeArguments;
                            var substitutedArgs = new TypeSymbol[typeArguments.Length];
                            bool changed = false;

                            for (int i = 0; i < typeArguments.Length; i++)
                            {
                                var substituted = SubstituteType(typeArguments[i], substitutions, compilation);
                                substitutedArgs[i] = substituted;
                                changed |= !SymbolEqualityComparer.Default.Equals(substituted, typeArguments[i]);
                            }

                            if (changed && named.ConstructedFrom is NamedTypeSymbol definition)
                            {
                                return (NamedTypeSymbol)definition.Construct(substitutedArgs);
                            }

                            return named;
                        }

                    case ArrayTypeSymbol arrayType:
                        {
                            var substitutedElement = SubstituteType(arrayType.ElementType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, arrayType.ElementType))
                            {
                                return compilation.CreateArrayTypeSymbol(substitutedElement, arrayType.Rank, arrayType.FixedLength);
                            }

                            return arrayType;
                        }

                    case NullableTypeSymbol nullableType:
                        {
                            var substitutedUnderlying = SubstituteType(nullableType.UnderlyingType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedUnderlying, nullableType.UnderlyingType))
                            {
                                return substitutedUnderlying.ApplySubstitutedNullability(nullableType);
                            }

                            return nullableType;
                        }

                    case RefTypeSymbol refTypeType:
                        {
                            var substitutedElement = SubstituteType(refTypeType.ElementType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, refTypeType.ElementType))
                            {
                                return new RefTypeSymbol(substitutedElement);
                            }

                            return refTypeType;
                        }

                    case AddressTypeSymbol addressType:
                        {
                            var substitutedElement = SubstituteType(addressType.ReferencedType, substitutions, compilation);
                            if (!SymbolEqualityComparer.Default.Equals(substitutedElement, addressType.ReferencedType))
                            {
                                return new AddressTypeSymbolImplementation(substitutedElement);
                            }

                            return (TypeSymbol)addressType;
                        }
                }

                return sourceType;
            }
        }
    }
}
