using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal sealed class ConstructedNamedTypeSymbol : NamedTypeSymbol, UnionSymbol, UnionCaseTypeSymbol, IConstructedTypeSubstitutionInfo, INestedTypeDiscovery, IExtensionTypeInfo
{
    private readonly NamedTypeSymbol _originalDefinition;
    private readonly Dictionary<TypeParameterSymbol, TypeSymbol> _substitutionMap;
    private readonly NamedTypeSymbol? _containingTypeOverride;
    private ImmutableArray<Symbol>? _members;
    private readonly ConcurrentDictionary<string, ImmutableArray<Symbol>> _membersByName = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<Symbol, Symbol> _substitutedMembers = new(ReferenceEqualityComparer.Instance);
    private ImmutableArray<FieldSymbol>? _tupleElements;
    private ImmutableArray<NamedTypeSymbol>? _interfaces;
    private ImmutableArray<NamedTypeSymbol>? _allInterfaces;
    private ImmutableArray<TypeSymbol>? _variants;
    private ImmutableArray<UnionCaseTypeSymbol>? _declaredCases;
    private ImmutableArray<TypeSymbol>? _memberTypes;
    private ImmutableArray<ParameterSymbol>? _constructorParameters;
    private ImmutableArray<TypeParameterSymbol> _typeParameters;
    private NamedTypeSymbol? _baseType;
    private UnionSymbol? _union;
    private FieldSymbol? _discriminatorField;
    private FieldSymbol? _payloadField;

    private readonly ImmutableArray<TypeSymbol> _explicitTypeArguments;
    private ImmutableArray<TypeSymbol> _typeArguments;
    private ImmutableArray<TypeSymbol> _allTypeArguments;
    private TypeArgumentsComputationState _typeArgumentsState;

    private enum TypeArgumentsComputationState
    {
        Uninitialized = 0,
        Computing = 1,
        Computed = 2
    }

    public ConstructedNamedTypeSymbol(NamedTypeSymbol originalDefinition, ImmutableArray<TypeSymbol> typeArguments)
        : this(originalDefinition, typeArguments, inheritedSubstitution: null, containingTypeOverride: null)
    {
    }

    private static Dictionary<TypeParameterSymbol, TypeSymbol> CreateSubstitutionMap(
     NamedTypeSymbol originalDefinition,
     ImmutableArray<TypeSymbol> typeArguments,
     Dictionary<TypeParameterSymbol, TypeSymbol>? inheritedSubstitution)
    {
        static TypeParameterSymbol NormalizeKey(TypeParameterSymbol parameter) =>
            (TypeParameterSymbol)(parameter.OriginalDefinition ?? parameter);

        var substitution = new Dictionary<TypeParameterSymbol, TypeSymbol>(
            ReferenceEqualityComparer.Instance);

        if (inheritedSubstitution is not null)
        {
            foreach (var (key, value) in inheritedSubstitution)
                substitution[NormalizeKey(key)] = value;
        }

        if (!typeArguments.IsDefaultOrEmpty)
        {
            var typeParameters = originalDefinition.TypeParameters;
            var argIndex = 0;

            foreach (var rawParam in typeParameters)
            {
                if (argIndex >= typeArguments.Length)
                    break;

                var parameter = NormalizeKey(rawParam);
                var argument = typeArguments[argIndex++];

                if (substitution.TryGetValue(parameter, out var existing))
                {
                    // If they’re logically equal, nothing to do
                    if (IsEquivalentForSubstitution(existing, argument))
                        continue;

                    // If we already have a concrete type and the new one is still a TP,
                    // keep the existing mapping.
                    if (existing is not TypeParameterSymbol && argument is TypeParameterSymbol)
                        continue;
                }

                substitution[parameter] = argument;
            }
        }

        return substitution;
    }

    private ConstructedNamedTypeSymbol(
        NamedTypeSymbol originalDefinition,
        ImmutableArray<TypeSymbol> typeArguments,
        Dictionary<TypeParameterSymbol, TypeSymbol>? inheritedSubstitution,
        NamedTypeSymbol? containingTypeOverride)
    {
        ConstructedFrom = originalDefinition;
        _originalDefinition = originalDefinition;
        _explicitTypeArguments = typeArguments.IsDefault ? ImmutableArray<TypeSymbol>.Empty : typeArguments;
        _containingTypeOverride = containingTypeOverride;

        _substitutionMap = CreateSubstitutionMap(originalDefinition, typeArguments, inheritedSubstitution);
        // Re-anchoring a nested type must close its member signatures as well as
        // its displayed owner, even when it declares no type parameters itself.
        TypeSubstitution.AddContainingTypeSubstitutions(containingTypeOverride, _substitutionMap);
    }

    public ImmutableArray<TypeSymbol> TypeArguments
    {
        get
        {
            if (_typeArgumentsState == TypeArgumentsComputationState.Computed)
                return _typeArguments;

            if (_typeArgumentsState == TypeArgumentsComputationState.Computing)
                return GetExplicitTypeArgumentsForInference();

            _typeArgumentsState = TypeArgumentsComputationState.Computing;
            try
            {
                _typeArguments = BuildTypeArguments();
                _typeArgumentsState = TypeArgumentsComputationState.Computed;
                return _typeArguments;
            }
            catch
            {
                _typeArgumentsState = TypeArgumentsComputationState.Uninitialized;
                _typeArguments = default;
                throw;
            }
        }
    }

    internal ImmutableArray<TypeSymbol> GetExplicitTypeArgumentsForInference()
        => _explicitTypeArguments.IsDefault ? ImmutableArray<TypeSymbol>.Empty : _explicitTypeArguments;

    NamedTypeSymbol IConstructedTypeSubstitutionInfo.DefinitionForSubstitution => _originalDefinition;
    ImmutableArray<TypeSymbol> IConstructedTypeSubstitutionInfo.ExplicitTypeArgumentsForSubstitution => GetExplicitTypeArgumentsForInference();

    private bool TryGetSubstitution(TypeParameterSymbol parameter, out TypeSymbol replacement)
    {
        var lookup = (TypeParameterSymbol)(parameter.OriginalDefinition ?? parameter);
        if (_substitutionMap.TryGetValue(lookup, out replacement!))
            return true;

        foreach (var (key, value) in _substitutionMap)
        {
            var normalizedKey = (TypeParameterSymbol)(key.OriginalDefinition ?? key);
            if (normalizedKey.Ordinal == lookup.Ordinal && normalizedKey.Name == lookup.Name)
            {
                replacement = value;
                return true;
            }
        }

        replacement = null!;
        return false;
    }

    public TypeSymbol Substitute(
        TypeSymbol type,
        Dictionary<TypeParameterSymbol, TypeParameterSymbol>? methodMap = null)
    {
        var inProgress = new HashSet<TypeSymbol>(ReferenceEqualityComparer.Instance);
        var cache = new Dictionary<TypeSymbol, TypeSymbol>(ReferenceEqualityComparer.Instance);
        return ReanchorNestedTypeIfNeeded(SubstituteCore(type, methodMap, inProgress, cache), methodMap);
    }

    internal TypeSymbol ReanchorNestedTypeIfNeeded(
        TypeSymbol type,
        Dictionary<TypeParameterSymbol, TypeParameterSymbol>? methodMap = null)
    {
        if (type is NamedTypeSymbol { ContainingType: { } anchoredContainingType } &&
            ReferenceEquals(anchoredContainingType, this))
        {
            return type;
        }

        if (type is NamedTypeSymbol namedSelf &&
            ReferenceEquals(TypeSubstitution.GetDefinitionForSubstitution(namedSelf), _originalDefinition) &&
            HaveEquivalentExplicitTypeArguments(namedSelf))
        {
            return this;
        }

        if (type is RefTypeSymbol refType)
        {
            var rewrittenElement = ReanchorNestedTypeIfNeeded(refType.ElementType, methodMap);
            return SymbolEqualityComparer.Default.Equals(rewrittenElement, refType.ElementType)
                ? type
                : new RefTypeSymbol(rewrittenElement);
        }

        if (type is NamedTypeSymbol named &&
            named.ContainingType is NamedTypeSymbol containing &&
            ReferenceEquals(TypeSubstitution.GetDefinitionForSubstitution(containing), _originalDefinition))
        {
            var nestedDefinition = TypeSubstitution.GetDefinitionForSubstitution(named);
            var nestedArguments = GetShallowTypeArguments(named);
            var substitutedArguments = nestedArguments.IsDefaultOrEmpty
                ? ImmutableArray<TypeSymbol>.Empty
                : ImmutableArray.CreateRange(nestedArguments.Select(argument => Substitute(argument, methodMap)));

            return new ConstructedNamedTypeSymbol(nestedDefinition, substitutedArguments, _substitutionMap, this);
        }

        return type;
    }

    private bool HaveEquivalentExplicitTypeArguments(NamedTypeSymbol candidate)
    {
        var candidateArguments = GetShallowTypeArguments(candidate);
        if (candidateArguments.Length != _explicitTypeArguments.Length)
            return false;

        for (var index = 0; index < candidateArguments.Length; index++)
        {
            if (!IsEquivalentForSubstitution(candidateArguments[index], _explicitTypeArguments[index]))
                return false;
        }

        return true;
    }


    private TypeSymbol SubstituteCore(
        TypeSymbol type,
        Dictionary<TypeParameterSymbol, TypeParameterSymbol>? methodMap,
        HashSet<TypeSymbol> inProgress,
        Dictionary<TypeSymbol, TypeSymbol> cache)
    {
        if (cache.TryGetValue(type, out var cached))
            return cached;

        if (!inProgress.Add(type))
            return type;

        try
        {
            TypeSymbol result = type;

            if (type is TypeParameterSymbol tp)
            {
                if (methodMap is not null && methodMap.TryGetValue(tp, out var mappedMethodParameter))
                {
                    result = mappedMethodParameter;
                    cache[type] = result;
                    return result;
                }

                if (TryGetSubstitution(tp, out var concrete))
                {
                    result = concrete;
                    cache[type] = result;
                    return result;
                }
            }

            if (type is NamedTypeSymbol nestedType &&
                nestedType.ContainingType is NamedTypeSymbol nestedContainingType &&
                ReferenceEquals(TypeSubstitution.GetDefinitionForSubstitution(nestedContainingType), _originalDefinition))
            {
                var nestedArguments = GetShallowTypeArguments(nestedType);
                var substitutedNestedArguments = nestedArguments.IsDefaultOrEmpty
                    ? ImmutableArray<TypeSymbol>.Empty
                    : ImmutableArray.CreateRange(nestedArguments.Select(argument => SubstituteCore(argument, methodMap, inProgress, cache)));

                var nestedDefinition = TypeSubstitution.GetDefinitionForSubstitution(nestedType);
                result = new ConstructedNamedTypeSymbol(nestedDefinition, substitutedNestedArguments, _substitutionMap, this);
                cache[type] = result;
                return result;
            }

            if (type is FunctionReturnTypeSymbolImplementation projection)
            {
                result = projection.Substitute(t => SubstituteCore(t, methodMap, inProgress, cache));
                cache[type] = result;
                return result;
            }

            if (type is StructuralFunctionTypeSymbol function)
            {
                result = function.Substitute(t => SubstituteCore(t, methodMap, inProgress, cache));
                cache[type] = result;
                return result;
            }

            if (type is StructuralUnionTypeSymbol union)
            {
                result = TypeSubstitution.SubstituteStructuralUnion(union,
                    constituent => SubstituteCore(constituent, methodMap, inProgress, cache));
                cache[type] = result;
                return result;
            }

            if (type is IntersectionTypeSymbol intersection)
            {
                result = TypeSubstitution.SubstituteIntersection(intersection,
                    constituent => SubstituteCore(constituent, methodMap, inProgress, cache));
                cache[type] = result;
                return result;
            }

            if (type is NullableTypeSymbol nullableTypeSymbol)
            {
                var underlyingType = SubstituteCore(nullableTypeSymbol.UnderlyingType, methodMap, inProgress, cache);

                if (!IsEquivalentForSubstitution(underlyingType, nullableTypeSymbol.UnderlyingType))
                    result = underlyingType.ApplySubstitutedNullability(nullableTypeSymbol);
                else
                    result = type;

                cache[type] = result;
                return result;
            }

            if (type is RefTypeSymbol refType)
            {
                var substitutedElement = SubstituteCore(refType.ElementType, methodMap, inProgress, cache);

                if (!IsEquivalentForSubstitution(substitutedElement, refType.ElementType))
                    result = new RefTypeSymbol(substitutedElement);
                else
                    result = type;

                cache[type] = result;
                return result;
            }

            if (type is AddressTypeSymbol address)
            {
                var substitutedElement = SubstituteCore(address.ReferencedType, methodMap, inProgress, cache);

                if (!IsEquivalentForSubstitution(substitutedElement, address.ReferencedType))
                    result = new AddressTypeSymbolImplementation(substitutedElement);
                else
                    result = type;

                cache[type] = result;
                return result;
            }

            if (type is PointerTypeSymbol pointerType)
            {
                var element = SubstituteCore(pointerType.PointedAtType, methodMap, inProgress, cache);
                result = IsEquivalentForSubstitution(element, pointerType.PointedAtType)
                    ? type : new PointerTypeSymbolImplementation(element);
                cache[type] = result;
                return result;
            }

            if (type is ArrayTypeSymbol arrayType)
            {
                var substitutedElement = SubstituteCore(arrayType.ElementType, methodMap, inProgress, cache);

                if (!IsEquivalentForSubstitution(substitutedElement, arrayType.ElementType))
                    result = new ArrayTypeSymbolImplementation(arrayType.BaseType, substitutedElement, arrayType.ContainingSymbol, arrayType.ContainingType, arrayType.ContainingNamespace, [], arrayType.Rank, arrayType.FixedLength);
                else
                    result = type;

                cache[type] = result;
                return result;
            }

            if (type is TupleTypeSymbol tupleType)
            {
                result = TypeSubstitution.SubstituteTupleElements(
                    tupleType,
                    element => SubstituteCore(element, methodMap, inProgress, cache));
                cache[type] = result;
                return result;
            }

            if (type is NamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var typeArguments = GetShallowTypeArguments(named);
                var substitutedArgs = new TypeSymbol[typeArguments.Length];
                var changed = false;

                for (int i = 0; i < typeArguments.Length; i++)
                {
                    var originalArg = typeArguments[i];
                    var substitutedArg = SubstituteCore(originalArg, methodMap, inProgress, cache);

                    substitutedArgs[i] = substitutedArg;

                    if (!IsEquivalentForSubstitution(substitutedArg, originalArg))
                        changed = true;
                }

                if ((named.ConstructedFrom ?? named).SpecialType == SpecialType.System_Nullable_T &&
                    substitutedArgs.Length == 1)
                {
                    result = substitutedArgs[0].GetNullableType();
                    cache[type] = result;
                    return result;
                }

                if (!changed)
                {
                    // Even if generic args did not change, we may still need to re-anchor a nested type
                    // under a substituted containing type.
                    if (named.ContainingType is NamedTypeSymbol && TryGetContainingOverride(named, out var containingOverride) && containingOverride is not null)
                    {
                        if (named is ConstructedNamedTypeSymbol existingConstructed &&
                            existingConstructed.ContainingType is NamedTypeSymbol existingContaining &&
                            AreNamedTypesEquivalentShallow(existingContaining, containingOverride))
                        {
                            result = named;
                        }
                        else
                        {
                            result = new ConstructedNamedTypeSymbol((NamedTypeSymbol?)(named.ConstructedFrom ?? named) ?? named, typeArguments, _substitutionMap, containingOverride);
                        }
                    }
                    else
                    {
                        result = named;
                    }

                    cache[type] = result;
                    return result;
                }

                // Avoid reusing a possibly already-constructed named
                var constructedFrom = (NamedTypeSymbol?)named.ConstructedFrom ?? named;

                // If this is nested and the containing type must be substituted, create a constructed wrapper
                // so ContainingType/ContainingSymbol reflect the constructed outer.
                if (named.ContainingType is NamedTypeSymbol && TryGetContainingOverride(named, out var overrideContaining) && overrideContaining is not null)
                {
                    var immutableArguments = ImmutableArray.Create(substitutedArgs);
                    if (named is ConstructedNamedTypeSymbol existingConstructed &&
                        existingConstructed.ContainingType is NamedTypeSymbol existingContaining &&
                        AreNamedTypesEquivalentShallow(existingContaining, overrideContaining))
                    {
                        result = named;
                    }
                    else
                    {
                        result = new ConstructedNamedTypeSymbol(constructedFrom, immutableArguments, _substitutionMap, overrideContaining);
                    }

                    cache[type] = result;
                    return result;
                }

                result = constructedFrom.Construct(substitutedArgs);
                cache[type] = result;
                return result;
            }

            // Nested non-generic named types (e.g. Result<T,E>.Ok) must still be re-anchored under a substituted containing type.
            if (type is NamedTypeSymbol nestedNamed && nestedNamed.ContainingType is NamedTypeSymbol)
            {
                if (TryGetContainingOverride(nestedNamed, out var containingOverride) && containingOverride is not null)
                {
                    if (nestedNamed is ConstructedNamedTypeSymbol existingConstructed &&
                        existingConstructed.ContainingType is NamedTypeSymbol existingContaining &&
                        AreNamedTypesEquivalentShallow(existingContaining, containingOverride))
                    {
                        result = nestedNamed;
                    }
                    else
                    {
                        result = new ConstructedNamedTypeSymbol(nestedNamed, ImmutableArray<TypeSymbol>.Empty, _substitutionMap, containingOverride);
                    }

                    cache[type] = result;
                    return result;
                }
            }

            cache[type] = result;
            return result;
        }
        finally
        {
            inProgress.Remove(type);
        }
    }

    private static bool IsEquivalentForSubstitution(TypeSymbol left, TypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (left is TypeParameterSymbol leftParameter && right is TypeParameterSymbol rightParameter)
        {
            var leftDefinition = (TypeParameterSymbol)(leftParameter.OriginalDefinition ?? leftParameter);
            var rightDefinition = (TypeParameterSymbol)(rightParameter.OriginalDefinition ?? rightParameter);
            if (ReferenceEquals(leftDefinition, rightDefinition))
                return true;

            if (leftDefinition.OwnerKind != rightDefinition.OwnerKind)
                return false;

            if (leftDefinition.Ordinal != rightDefinition.Ordinal)
                return false;

            if (!string.Equals(leftDefinition.Name, rightDefinition.Name, StringComparison.Ordinal))
                return false;

            if (leftDefinition.OwnerKind == TypeParameterOwnerKind.Method)
            {
                var leftOwner = (MethodSymbol?)(leftDefinition.DeclaringMethodParameterOwner?.OriginalDefinition ?? leftDefinition.DeclaringMethodParameterOwner);
                var rightOwner = (MethodSymbol?)(rightDefinition.DeclaringMethodParameterOwner?.OriginalDefinition ?? rightDefinition.DeclaringMethodParameterOwner);
                return SymbolEqualityComparer.Default.Equals(leftOwner, rightOwner);
            }

            var leftTypeOwner = (NamedTypeSymbol?)(leftDefinition.DeclaringTypeParameterOwner?.OriginalDefinition ?? leftDefinition.DeclaringTypeParameterOwner);
            var rightTypeOwner = (NamedTypeSymbol?)(rightDefinition.DeclaringTypeParameterOwner?.OriginalDefinition ?? rightDefinition.DeclaringTypeParameterOwner);
            return SymbolEqualityComparer.Default.Equals(leftTypeOwner, rightTypeOwner);
        }

        if (left is NamedTypeSymbol leftNamed && right is NamedTypeSymbol rightNamed)
        {
            var leftDefinition = (NamedTypeSymbol)(leftNamed.OriginalDefinition ?? leftNamed);
            var rightDefinition = (NamedTypeSymbol)(rightNamed.OriginalDefinition ?? rightNamed);

            // Definitions must match.
            if (!string.Equals(leftDefinition.MetadataName, rightDefinition.MetadataName, StringComparison.Ordinal))
                return false;

            if (leftDefinition.Arity != rightDefinition.Arity)
                return false;

            // For non-generic types, or when no type arguments are present, definition+arity is enough.
            if (!leftNamed.IsGenericType || leftNamed.IsUnboundGenericType || leftDefinition.Arity == 0)
                return true;

            // Compare shallow type arguments. This is required for correct change detection during substitution
            // (e.g. IEnumerable<KeyValuePair<TKey,TValue>> must become IEnumerable<KeyValuePair<string,int>>).
            var leftArgs = GetShallowTypeArguments(leftNamed);
            var rightArgs = GetShallowTypeArguments(rightNamed);

            if (leftArgs.IsDefaultOrEmpty || rightArgs.IsDefaultOrEmpty)
                return leftArgs.Length == rightArgs.Length;

            if (leftArgs.Length != rightArgs.Length)
                return false;

            for (var i = 0; i < leftArgs.Length; i++)
            {
                if (!IsEquivalentForSubstitution(leftArgs[i], rightArgs[i]))
                    return false;
            }

            return true;
        }

        return false;
    }

    private static bool AreNamedTypesEquivalentShallow(NamedTypeSymbol left, NamedTypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        var leftDefinition = (NamedTypeSymbol)(left.OriginalDefinition ?? left);
        var rightDefinition = (NamedTypeSymbol)(right.OriginalDefinition ?? right);

        if (!ReferenceEquals(leftDefinition, rightDefinition))
            return false;

        if (left.Arity != right.Arity)
            return false;

        var leftArgs = GetShallowTypeArguments(left);
        var rightArgs = GetShallowTypeArguments(right);

        if (leftArgs.IsDefaultOrEmpty || rightArgs.IsDefaultOrEmpty)
            return leftArgs.Length == rightArgs.Length;

        if (leftArgs.Length != rightArgs.Length)
            return false;

        for (var i = 0; i < leftArgs.Length; i++)
        {
            if (!ReferenceEquals(leftArgs[i], rightArgs[i]))
                return false;
        }

        return true;
    }

    private static ImmutableArray<TypeSymbol> GetShallowTypeArguments(NamedTypeSymbol type)
    {
        return TypeSubstitution.GetShallowTypeArguments(type);
    }

    internal static NamedTypeSymbol ReanchorNested(
        NamedTypeSymbol nestedDefinition,
        NamedTypeSymbol containingOverride,
        Dictionary<TypeParameterSymbol, TypeSymbol>? inheritedSubstitution,
        ImmutableArray<TypeSymbol> typeArguments)
    {
        return new ConstructedNamedTypeSymbol(nestedDefinition, typeArguments, inheritedSubstitution, containingOverride);
    }

    private ImmutableArray<NamedTypeSymbol> BuildSubstitutedInterfaceSet(ImmutableArray<NamedTypeSymbol> interfaces)
    {
        if (interfaces.IsDefaultOrEmpty || interfaces.Length == 0)
            return interfaces;

        var cache = new Dictionary<NamedTypeSymbol, NamedTypeSymbol>(ReferenceEqualityComparer.Instance);
        var visiting = new HashSet<NamedTypeSymbol>(ReferenceEqualityComparer.Instance);
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>(interfaces.Length);

        foreach (var interfaceType in interfaces)
        {
            var result = SubstituteInterfaceType(interfaceType, cache, visiting);
            builder.Add(result);
        }

        return builder.MoveToImmutable();
    }

    private NamedTypeSymbol SubstituteInterfaceType(
        NamedTypeSymbol interfaceType,
        Dictionary<NamedTypeSymbol, NamedTypeSymbol> cache,
        HashSet<NamedTypeSymbol> visiting)
    {
        if (cache.TryGetValue(interfaceType, out var cached))
        {
            return cached;
        }

        if (!visiting.Add(interfaceType))
        {
            return interfaceType;
        }

        try
        {
            if (!interfaceType.IsGenericType || interfaceType.IsUnboundGenericType)
            {
                cache[interfaceType] = interfaceType;
                return interfaceType;
            }

            var originalArguments = interfaceType.TypeArguments;
            var substitutedArguments = new TypeSymbol[originalArguments.Length];
            var changed = false;

            for (var i = 0; i < originalArguments.Length; i++)
            {
                var original = originalArguments[i];
                var substituted = SubstituteInterfaceTypeArgument(original, cache, visiting);
                substitutedArguments[i] = substituted;

                if (!IsEquivalentForSubstitution(substituted, original))
                    changed = true;
            }

            if (!changed)
            {
                cache[interfaceType] = interfaceType;
                return interfaceType;
            }

            var definition = (NamedTypeSymbol?)(interfaceType.ConstructedFrom ?? interfaceType) ?? interfaceType;
            var constructed = (NamedTypeSymbol)definition.Construct(substitutedArguments);
            cache[interfaceType] = constructed;
            return constructed;
        }
        finally
        {
            visiting.Remove(interfaceType);
        }
    }

    private TypeSymbol SubstituteInterfaceTypeArgument(
        TypeSymbol argument,
        Dictionary<NamedTypeSymbol, NamedTypeSymbol> cache,
        HashSet<NamedTypeSymbol> visiting)
    {
        if (argument is TypeParameterSymbol typeParameter && TryGetSubstitution(typeParameter, out var replacement))
            return replacement;

        if (argument is NamedTypeSymbol namedArg)
        {
            // Non-generic named types (e.g. string, int) contain no type parameters —
            // return unchanged.  For generic named types (e.g. KeyValuePair<TKey, TValue>)
            // we must recurse so that inner type parameters get substituted as well.
            if (!namedArg.IsGenericType || namedArg.TypeArguments.IsDefaultOrEmpty)
                return argument;

            return SubstituteInterfaceType(namedArg, cache, visiting);
        }

        if (argument is NullableTypeSymbol nullableArgument)
        {
            var underlying = SubstituteInterfaceTypeArgument(nullableArgument.UnderlyingType, cache, visiting);
            return IsEquivalentForSubstitution(underlying, nullableArgument.UnderlyingType)
                ? argument
                : underlying.ApplySubstitutedNullability(nullableArgument);
        }

        if (argument is ArrayTypeSymbol arrayArgument)
        {
            var element = SubstituteInterfaceTypeArgument(arrayArgument.ElementType, cache, visiting);
            if (IsEquivalentForSubstitution(element, arrayArgument.ElementType))
                return argument;

            return new ArrayTypeSymbolImplementation(
                arrayArgument.BaseType,
                element,
                arrayArgument.ContainingSymbol,
                arrayArgument.ContainingType,
                arrayArgument.ContainingNamespace,
                [],
                arrayArgument.Rank,
                arrayArgument.FixedLength);
        }

        if (argument is RefTypeSymbol refTypeArgument)
        {
            var element = SubstituteInterfaceTypeArgument(refTypeArgument.ElementType, cache, visiting);
            return IsEquivalentForSubstitution(element, refTypeArgument.ElementType)
                ? argument
                : new RefTypeSymbol(element);
        }

        if (argument is AddressTypeSymbol addressArgument)
        {
            var element = SubstituteInterfaceTypeArgument(addressArgument.ReferencedType, cache, visiting);
            return IsEquivalentForSubstitution(element, addressArgument.ReferencedType)
                ? argument
                : new AddressTypeSymbolImplementation(element);
        }

        return argument;
    }

    TypeSymbol? IExtensionTypeInfo.ExtensionReceiverType
    {
        get
        {
            var receiver = _originalDefinition switch
            {
                IExtensionTypeInfo info => info.ExtensionReceiverType,
                SourceNamedTypeSymbol source => source.ExtensionReceiverType,
                _ => null
            };
            return receiver is null ? null : Substitute(receiver);
        }
    }

    bool IExtensionTypeInfo.HasMemberLevelExtensions
        => _originalDefinition is IExtensionTypeInfo { HasMemberLevelExtensions: true };

    IEnumerable<NamedTypeSymbol> INestedTypeDiscovery.GetNestedTypesForDiscovery()
        => _originalDefinition is INestedTypeDiscovery discovery
            ? discovery.GetNestedTypesForDiscovery()
            : GetMembers().OfType<NamedTypeSymbol>();

    public ImmutableArray<Symbol> GetMembers()
    {
        if (!HasMutableSourceDeclarationState() && _members is { } cachedMembers)
            return cachedMembers;

        var substitutedMembers = _originalDefinition.GetMembers().Select(SubstituteMember).ToImmutableArray();

        if (HasMutableSourceDeclarationState())
            return substitutedMembers;

        return _members ??= substitutedMembers;
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        if (string.IsNullOrEmpty(name))
            return GetMembers();

        if (HasMutableSourceDeclarationState())
            return BuildMembersByName(name);

        return _membersByName.GetOrAdd(name, BuildMembersByName);

        ImmutableArray<Symbol> BuildMembersByName(string memberName)
        {
            return _originalDefinition.GetMembers(memberName)
                .Select(SubstituteMember)
                .Where(member => string.Equals(member.Name, memberName, StringComparison.Ordinal))
                .ToImmutableArray();
        }
    }

    internal ImmutableArray<TypeSymbol> GetAllTypeArguments() =>
        _allTypeArguments.IsDefault ? _allTypeArguments = BuildAllTypeArguments() : _allTypeArguments;

    private ImmutableArray<TypeSymbol> BuildAllTypeArguments()
    {
        var selfArgs = TypeArguments;

        // Union cases are semantically nested in their union, but their CLR type is nested
        // in either the non-generic union or a non-generic companion. The semantic owner's
        // type arguments must therefore never be inherited into the case's runtime identity.
        if (TryGetCaseDefinition(out _))
            return selfArgs;

        // An open source owner contributes parameters too. Synthesized nested
        // state machines commonly have no constructed-owner override, but CLI
        // signatures still require all enclosing arguments before their own.
        if (ContainingType is { } containingType)
        {
            // Extension type parameters are emitted on methods, not on the
            // static container. Closures already carry their method aliases.
            if (containingType.OriginalDefinition is SourceNamedTypeSymbol { IsExtensionDeclaration: true })
                return selfArgs;

            var outerArgs = containingType is ConstructedNamedTypeSymbol constructedContaining
                ? constructedContaining.GetAllTypeArguments()
                : new ConstructedNamedTypeSymbol(containingType, containingType.TypeParameters.Cast<TypeSymbol>().ToImmutableArray()).GetAllTypeArguments();
            if (selfArgs.IsDefaultOrEmpty || selfArgs.Length == 0)
                return outerArgs;

            return outerArgs.AddRange(selfArgs);
        }

        return selfArgs;
    }

    private ImmutableArray<TypeSymbol> BuildTypeArguments()
    {
        // The constructor receives explicit type arguments for this constructed symbol.
        // Re-substituting them here can apply the same mapping twice (for example T[] -> T[][])
        // when the argument was already produced through substitution.
        return _explicitTypeArguments.IsDefault ? ImmutableArray<TypeSymbol>.Empty : _explicitTypeArguments;
    }

    private ImmutableArray<TypeParameterSymbol> BuildTypeParameters()
    {
        // Nested types do not implicitly inherit containing type parameters, but
        // their own constraints can mention parameters from a constructed outer
        // type. Keep definition ownership while projecting those constraint uses.
        var originalParameters = _originalDefinition.TypeParameters;
        if (originalParameters.IsDefaultOrEmpty)
            return originalParameters;

        ImmutableArray<TypeParameterSymbol>.Builder? builder = null;
        for (var i = 0; i < originalParameters.Length; i++)
        {
            var parameter = originalParameters[i];
            var constraints = parameter.ConstraintTypes;
            if (constraints.IsDefaultOrEmpty)
            {
                builder?.Add(parameter);
                continue;
            }

            var substitutedConstraints = ImmutableArray.CreateRange(
                constraints.Select(constraint => Substitute(constraint)));
            var changed = false;
            for (var constraintIndex = 0; constraintIndex < constraints.Length; constraintIndex++)
            {
                if (!SymbolEqualityComparer.Default.Equals(constraints[constraintIndex], substitutedConstraints[constraintIndex]))
                {
                    changed = true;
                    break;
                }
            }

            if (!changed)
            {
                builder?.Add(parameter);
                continue;
            }

            if (builder is null)
            {
                builder = ImmutableArray.CreateBuilder<TypeParameterSymbol>(originalParameters.Length);
                for (var previous = 0; previous < i; previous++)
                    builder.Add(originalParameters[previous]);
            }

            builder.Add(new SubstitutedNamedTypeParameterSymbol(parameter, substitutedConstraints));
        }

        return builder?.MoveToImmutable() ?? originalParameters;
    }

    // Helper methods for chain-aware substitution of nested types
    private NamedTypeSymbol? SubstituteContainingType(NamedTypeSymbol? containing)
    {
        if (containing is null)
            return null;

        // Substitute the containing type using the current substitution map.
        // This will walk outward (recursively) and produce the constructed outer chain when needed.
        var substituted = Substitute(containing) as NamedTypeSymbol;

        if (substituted is null)
            return containing;

        return substituted;
    }

    private bool TryGetContainingOverride(NamedTypeSymbol namedType, out NamedTypeSymbol? containingOverride)
    {
        containingOverride = null;

        if (namedType.ContainingType is not NamedTypeSymbol containing)
            return false;

        var substitutedContaining = SubstituteContainingType(containing);

        if (substitutedContaining is null)
            return false;

        // If the containing type changed due to substitution (or the symbol is logically the same but a different instance),
        // we must re-anchor the nested type under the substituted containing type.
        if (!AreNamedTypesEquivalentShallow(substitutedContaining, containing))
        {
            containingOverride = substitutedContaining;
            return true;
        }

        var containingDefinition = TypeSubstitution.GetDefinitionForSubstitution(containing);
        if (ReferenceEquals(containingDefinition, _originalDefinition))
        {
            containingOverride = this;
            return true;
        }

        // Also re-anchor if the nested type is directly under the original definition and we are the constructed instance.
        if (_containingTypeOverride is null && AreNamedTypesEquivalentShallow(containing, _originalDefinition))
        {
            containingOverride = this;
            return true;
        }

        return false;
    }

    private Symbol SubstituteMember(Symbol member)
        => HasMutableSourceDeclarationState()
            ? CreateSubstitutedMember(member)
            : _substitutedMembers.GetOrAdd(member, static (original, owner) => owner.CreateSubstitutedMember(original), this);

    private Symbol CreateSubstitutedMember(Symbol member) => member switch
    {
        MethodSymbol m => new SubstitutedMethodSymbol(m, this),
        FieldSymbol f => new SubstitutedFieldSymbol(f, this),
        PropertySymbol p => new SubstitutedPropertySymbol(p, this),
        EventSymbol e => new SubstitutedEventSymbol(e, this),
        NamedTypeSymbol t => SubstituteNamedType(t),
        _ => member
    };

    private NamedTypeSymbol SubstituteNamedType(NamedTypeSymbol namedType)
    {
        // Determine whether this type must be re-anchored under a substituted containing type.
        NamedTypeSymbol? containingOverride = null;
        _ = TryGetContainingOverride(namedType, out containingOverride);

        // Non-generic nested types (arity 0) still require a constructed wrapper when the containing type changes.
        if (namedType.Arity == 0)
        {
            return containingOverride is not null
                ? new ConstructedNamedTypeSymbol(namedType, ImmutableArray<TypeSymbol>.Empty, _substitutionMap, containingOverride)
                : namedType;
        }

        var typeParameters = namedType.TypeParameters;
        if (typeParameters.Length == 0)
        {
            return containingOverride is not null
                ? new ConstructedNamedTypeSymbol(namedType, ImmutableArray<TypeSymbol>.Empty, _substitutionMap, containingOverride)
                : namedType;
        }

        // Compatibility bridge: until all construction/binding paths are fully case-generic-first,
        // allow legacy projection of case parameters from substituted union arguments, but only when
        // the case's generic parameter names are coupled to the union's generic parameter names.
        ImmutableArray<TypeParameterSymbol> unionTypeParameters = ImmutableArray<TypeParameterSymbol>.Empty;
        ImmutableArray<TypeSymbol> unionTypeArguments = ImmutableArray<TypeSymbol>.Empty;
        UnionCaseTypeSymbol? unionCaseSymbol = null;
        if (namedType is UnionCaseTypeSymbol caseSymbol)
        {
            unionCaseSymbol = caseSymbol;
            var unionDefinition = TypeSubstitution.GetDefinitionForSubstitution((NamedTypeSymbol)caseSymbol.Union);
            var substitutedUnion = SubstituteNamedType(unionDefinition);
            unionTypeParameters = unionDefinition.TypeParameters;
            unionTypeArguments = substitutedUnion.TypeArguments;
        }

        var typeArguments = new TypeSymbol[typeParameters.Length];
        var changed = false;

        for (var i = 0; i < typeParameters.Length; i++)
        {
            var parameter = typeParameters[i];
            if (TryGetSubstitution(parameter, out var replacement))
            {
                typeArguments[i] = replacement;
                if (!IsEquivalentForSubstitution(replacement, parameter))
                    changed = true;
            }
            else if (unionCaseSymbol is not null &&
                UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(
                    unionCaseSymbol,
                    parameter,
                    unionTypeParameters,
                    unionTypeArguments,
                    out var projectedType))
            {
                typeArguments[i] = projectedType;
                if (!IsEquivalentForSubstitution(projectedType, parameter))
                    changed = true;
            }
            else
            {
                typeArguments[i] = parameter;
            }
        }

        // If nothing changed and there is no containing override, reuse the existing symbol.
        if (!changed && containingOverride is null)
            return namedType;

        var immutableArguments = ImmutableArray.Create(typeArguments);

        if (containingOverride is null)
            return (NamedTypeSymbol)namedType.Construct(typeArguments);

        return new ConstructedNamedTypeSymbol(namedType, immutableArguments, _substitutionMap, containingOverride);
    }

    private bool TryGetUnionDefinition(out UnionSymbol unionDefinition)
    {
        if (_originalDefinition is UnionSymbol union)
        {
            unionDefinition = union;
            return true;
        }

        if (ConstructedFrom is UnionSymbol constructedUnion)
        {
            unionDefinition = constructedUnion;
            return true;
        }

        unionDefinition = null!;
        return false;
    }

    private bool TryGetCaseDefinition(out UnionCaseTypeSymbol caseDefinition)
    {
        if (_originalDefinition is UnionCaseTypeSymbol caseSymbol)
        {
            caseDefinition = caseSymbol;
            return true;
        }

        if (ConstructedFrom is UnionCaseTypeSymbol constructedCase)
        {
            caseDefinition = constructedCase;
            return true;
        }

        caseDefinition = null!;
        return false;
    }

    // Symbol metadata forwarding
    public string Name => _originalDefinition.Name;
    public string MetadataName => _originalDefinition.MetadataName;
    public SymbolKind Kind => _originalDefinition.Kind;
    public TypeKind TypeKind
    {
        get
        {
            if (ConstructedFrom is NamedTypeSymbol constructedFrom)
                return constructedFrom.TypeKind;

            return _originalDefinition.TypeKind;
        }
    }
    public SpecialType SpecialType => _originalDefinition.SpecialType;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => _originalDefinition.IsReferenceType;
    public bool IsValueType => _originalDefinition.IsValueType;
    public bool IsUnion => _originalDefinition.IsUnion;
    public bool IsUnionCase => _originalDefinition.IsUnionCase;
    public NamedTypeSymbol? UnderlyingUnionType => _originalDefinition.UnderlyingUnionType;
    public NamedTypeSymbol MetadataContainingType =>
        TryGetCaseDefinition(out var caseDefinition)
            ? caseDefinition.MetadataContainingType
            : _originalDefinition;
    public NamedTypeSymbol? ContainingType => _containingTypeOverride ?? _originalDefinition.ContainingType;
    public NamespaceSymbol? ContainingNamespace => _containingTypeOverride?.ContainingNamespace ?? _originalDefinition.ContainingNamespace;
    public Symbol? ContainingSymbol => _containingTypeOverride ?? _originalDefinition.ContainingSymbol;
    public AssemblySymbol? ContainingAssembly => _originalDefinition.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _originalDefinition.ContainingModule;
    public Accessibility DeclaredAccessibility => _originalDefinition.DeclaredAccessibility;
    public bool IsStatic => _originalDefinition.IsStatic;
    public bool IsImplicitlyDeclared => true;
    public bool CanBeReferencedByName => true;
    public ImmutableArray<Location> Locations => _originalDefinition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _originalDefinition.DeclaringSyntaxReferences;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _originalDefinition.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _originalDefinition.GetDocumentationComment();
    public int Arity => _originalDefinition.Arity;
    public ImmutableArray<TypeSymbol> GetTypeArguments() => TypeArguments;
    public TypeSymbol? OriginalDefinition => _originalDefinition;
    public NamedTypeSymbol? BaseType => _baseType ??= _originalDefinition.BaseType is { } baseType
        ? Substitute(baseType) as NamedTypeSymbol
        : null;
    public ImmutableArray<TypeParameterSymbol> TypeParameters =>
        _typeParameters.IsDefault ? _typeParameters = BuildTypeParameters() : _typeParameters;
    public TypeSymbol? ConstructedFrom { get; }
    public bool IsAbstract => _originalDefinition.IsAbstract;
    public bool IsClosed => _originalDefinition.IsClosed;
    public bool IsRefLikeType => _originalDefinition.IsRefLikeType;
    public bool IsReadOnly => _originalDefinition.IsReadOnly;
    public bool IsGenericType => _originalDefinition.IsGenericType;
    public bool IsUnboundGenericType => false;
    public ImmutableArray<NamedTypeSymbol> Interfaces =>
        HasMutableSourceDeclarationState()
            ? BuildSubstitutedInterfaceSet(_originalDefinition.Interfaces)
            : _interfaces ??= BuildSubstitutedInterfaceSet(_originalDefinition.Interfaces);
    public ImmutableArray<NamedTypeSymbol> AllInterfaces =>
        HasMutableSourceDeclarationState()
            ? BuildSubstitutedInterfaceSet(_originalDefinition.AllInterfaces)
            : _allInterfaces ??= BuildSubstitutedInterfaceSet(_originalDefinition.AllInterfaces);
    public ImmutableArray<TypeSymbol> Variants
    {
        get
        {
            if (!TryGetUnionDefinition(out var unionDefinition))
                return ImmutableArray<TypeSymbol>.Empty;

            if (!HasMutableSourceDeclarationState() && _variants is not null)
                return _variants.Value;

            var builder = ImmutableArray.CreateBuilder<TypeSymbol>(unionDefinition.Variants.Length);
            foreach (var variant in unionDefinition.Variants)
                builder.Add(Substitute(variant));

            var substitutedVariants = builder.MoveToImmutable();
            if (HasMutableSourceDeclarationState())
                return substitutedVariants;

            _variants = substitutedVariants;
            return _variants.Value;
        }
    }

    public ImmutableArray<UnionCaseTypeSymbol> DeclaredCaseTypes
    {
        get
        {
            if (!TryGetUnionDefinition(out var unionDefinition))
                return ImmutableArray<UnionCaseTypeSymbol>.Empty;

            var substitutedCases = SubstituteUnionCases(unionDefinition.DeclaredCaseTypes);

            if (HasMutableSourceDeclarationState())
                return substitutedCases;

            return _declaredCases ??= substitutedCases;
        }
    }
    public ImmutableArray<TypeSymbol> MemberTypes
    {
        get
        {
            if (!TryGetUnionDefinition(out var unionDefinition))
                return ImmutableArray<TypeSymbol>.Empty;

            if (!HasMutableSourceDeclarationState() && _memberTypes is not null)
                return _memberTypes.Value;

            var builder = ImmutableArray.CreateBuilder<TypeSymbol>(unionDefinition.MemberTypes.Length);
            foreach (var memberType in unionDefinition.MemberTypes)
                builder.Add(memberType is UnionCaseTypeSymbol { IsUnionCase: true } caseType
                    ? SubstituteNamedType(caseType)
                    : Substitute(memberType));

            var substitutedMembers = builder.MoveToImmutable();
            if (HasMutableSourceDeclarationState())
                return substitutedMembers;

            _memberTypes = substitutedMembers;
            return _memberTypes.Value;
        }
    }
    public bool ContentMayBeNull
    {
        get
        {
            if (!TryGetUnionDefinition(out var unionDefinition))
                return MemberTypes.Any(UnionContentNullability.IsNullableContentType);

            return unionDefinition.ContentMayBeNull ||
                   MemberTypes.Any(UnionContentNullability.IsNullableContentType);
        }
    }

    public FieldSymbol DiscriminatorField
    {
        get
        {
            if (_discriminatorField is not null)
                return _discriminatorField;

            if (!TryGetUnionDefinition(out var unionDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union.");

            _discriminatorField = new SubstitutedFieldSymbol(unionDefinition.DiscriminatorField, this);
            return _discriminatorField;
        }
    }

    private bool HasMutableSourceDeclarationState()
        => _originalDefinition is SourceUnionSymbol or SourceUnionCaseTypeSymbol ||
            _originalDefinition.ContainingAssembly is SourceAssemblySymbol { Compilation.SourceDeclarationsComplete: false };
    public FieldSymbol PayloadField
    {
        get
        {
            if (_payloadField is not null)
                return _payloadField;

            if (!TryGetUnionDefinition(out var unionDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union.");

            _payloadField = new SubstitutedFieldSymbol(unionDefinition.PayloadField, this);
            return _payloadField;
        }
    }
    public ImmutableArray<MethodSymbol> Constructors => GetMembers().OfType<MethodSymbol>().Where(x => !x.IsStatic && x.IsConstructor).ToImmutableArray();
    public ImmutableArray<MethodSymbol> InstanceConstructors => Constructors;
    public MethodSymbol? StaticConstructor => GetMembers().OfType<MethodSymbol>().FirstOrDefault(x => x.MethodKind == MethodKind.StaticConstructor);

    public UnionSymbol Union
    {
        get
        {
            if (_union is not null)
                return _union;

            if (ContainingType is UnionSymbol containingUnion)
                return _union = containingUnion;

            if (!TryGetCaseDefinition(out var caseDefinition))
                throw new InvalidOperationException("Constructed type is not a discriminated union case.");

            return _union = (UnionSymbol)SubstituteNamedType((NamedTypeSymbol)caseDefinition.Union);
        }
    }

    public ImmutableArray<ParameterSymbol> ConstructorParameters
    {
        get
        {
            if (!TryGetCaseDefinition(out var caseDefinition))
                return ImmutableArray<ParameterSymbol>.Empty;

            return _constructorParameters ??= caseDefinition.ConstructorParameters
                .Select(p => (ParameterSymbol)new SubstitutedParameterSymbol(p, this))
                .ToImmutableArray();
        }
    }

    public int Ordinal => TryGetCaseDefinition(out var caseDefinition) ? caseDefinition.Ordinal : 0;

    public NamedTypeSymbol? UnderlyingTupleType
    {
        get
        {
            var underlying = _originalDefinition.UnderlyingTupleType;
            if (underlying is null)
                return null;

            if (AreNamedTypesEquivalentShallow(underlying, _originalDefinition))
                return this;

            return SubstituteNamedType(underlying);
        }
    }

    public ImmutableArray<FieldSymbol> TupleElements =>
        _tupleElements ??= _originalDefinition.TupleElements
            .Select(member => SubstituteMember(member))
            .OfType<FieldSymbol>()
            .ToImmutableArray();

    private ImmutableArray<UnionCaseTypeSymbol> SubstituteUnionCases(ImmutableArray<UnionCaseTypeSymbol> cases)
    {
        if (cases.IsDefaultOrEmpty || cases.Length == 0)
            return cases;

        var builder = ImmutableArray.CreateBuilder<UnionCaseTypeSymbol>(cases.Length);
        foreach (var caseSymbol in cases)
            builder.Add((UnionCaseTypeSymbol)SubstituteNamedType((NamedTypeSymbol)caseSymbol));

        return builder.MoveToImmutable();
    }

    public void Accept(SymbolVisitor visitor) => visitor.VisitNamedType(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitNamedType(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
    public TypeSymbol Construct(params TypeSymbol[] typeArguments)
    {
        if (_containingTypeOverride is null)
            return _originalDefinition.Construct(typeArguments);

        var immutableArguments = ImmutableArray.Create(typeArguments);
        return new ConstructedNamedTypeSymbol(_originalDefinition, immutableArguments, _substitutionMap, _containingTypeOverride);
    }

    public TypeSymbol? LookupType(string name)
    {
        return TypeLookupUtilities.SelectBestTypeByName(
            _originalDefinition.GetMembers(name)
                .OfType<NamedTypeSymbol>()
                .Select(SubstituteNamedType)
                .Cast<TypeSymbol>());
    }

    public string ToFullyQualifiedMetadataName()
    {
        var segments = new Stack<string>();

        for (NamedTypeSymbol? current = this; current is not null; current = GetMetadataContainingType(current))
        {
            var definition = (current as ConstructedNamedTypeSymbol)?._originalDefinition ?? current;
            var metadataName = definition.Name;
            var arity = definition.Arity;

            if (arity > 0)
                metadataName = $"{metadataName}`{arity}";

            segments.Push(metadataName);
        }

        static NamedTypeSymbol? GetMetadataContainingType(NamedTypeSymbol type)
            => type is UnionCaseTypeSymbol { IsUnionCase: true } unionCase
                ? unionCase.MetadataContainingType
                : type.ContainingType;

        var typeName = segments.Count > 0
            ? string.Join("+", segments)
            : MetadataName;

        var containingNamespace = ContainingNamespace;

        if (containingNamespace is null || containingNamespace.IsGlobalNamespace)
            return typeName;

        var namespaceName = containingNamespace.ToMetadataName();

        return string.IsNullOrEmpty(namespaceName)
            ? typeName
            : $"{namespaceName}.{typeName}";
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        foreach (var member in _originalDefinition.GetMembers(name))
        {
            if (member.Name == name)
            {
                symbol = SubstituteMember(member);
                return true;
            }
        }

        symbol = null;
        return false;
    }

    private string GetDebuggerDisplay()
    {
        try
        {
            return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}";
        }
        catch (Exception exc)
        {
            return $"{Kind}: <{exc.GetType().Name}>";
        }
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }
}

internal sealed class SubstitutedNamedTypeParameterSymbol : TypeParameterSymbol
{
    private readonly TypeParameterSymbol _original;
    private readonly ImmutableArray<TypeSymbol> _constraintTypes;
    private ImmutableArray<NamedTypeSymbol>? _interfaces;
    private ImmutableArray<NamedTypeSymbol>? _allInterfaces;

    public SubstitutedNamedTypeParameterSymbol(
        TypeParameterSymbol original,
        ImmutableArray<TypeSymbol> constraintTypes)
    {
        _original = original;
        _constraintTypes = constraintTypes;
    }

    public string Name => _original.Name;
    public string MetadataName => _original.MetadataName;
    public SymbolKind Kind => _original.Kind;
    public Symbol? ContainingSymbol => _original.ContainingSymbol;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _original.ContainingType;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => false;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public int Ordinal => _original.Ordinal;
    public TypeParameterOwnerKind OwnerKind => _original.OwnerKind;
    public NamedTypeSymbol? DeclaringTypeParameterOwner => _original.DeclaringTypeParameterOwner;
    public MethodSymbol? DeclaringMethodParameterOwner => null;
    public TypeParameterConstraintKind ConstraintKind => _original.ConstraintKind;
    public ImmutableArray<TypeSymbol> ConstraintTypes => _constraintTypes;
    public VarianceKind Variance => _original.Variance;

    public NamedTypeSymbol? BaseType => _constraintTypes
        .OfType<NamedTypeSymbol>()
        .FirstOrDefault(static constraint => constraint.TypeKind == TypeKind.Class) ?? _original.BaseType;
    public TypeSymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public SpecialType SpecialType => _original.SpecialType;
    public TypeKind TypeKind => _original.TypeKind;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => _original.IsReferenceType;
    public bool IsValueType => _original.IsValueType;
    public bool IsInterface => _original.IsInterface;
    public bool IsTupleType => _original.IsTupleType;
    public bool IsUnion => _original.IsUnion;
    public bool IsUnionCase => _original.IsUnionCase;
    public NamedTypeSymbol? UnderlyingUnionType => _original.UnderlyingUnionType;

    public ImmutableArray<NamedTypeSymbol> Interfaces =>
        _interfaces ??= BuildInterfaces();

    public ImmutableArray<NamedTypeSymbol> AllInterfaces
    {
        get
        {
            if (_allInterfaces.HasValue)
                return _allInterfaces.Value;

            var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
            var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var direct in Interfaces)
            {
                if (seen.Add(direct))
                    builder.Add(direct);

                foreach (var inherited in direct.AllInterfaces)
                {
                    if (seen.Add(inherited))
                        builder.Add(inherited);
                }
            }

            _allInterfaces = builder.MoveToImmutable();
            return _allInterfaces.Value;
        }
    }

    public ImmutableArray<Symbol> GetMembers() => _original.GetMembers();
    public ImmutableArray<Symbol> GetMembers(string name) => _original.GetMembers(name);
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    private ImmutableArray<NamedTypeSymbol> BuildInterfaces()
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var constraint in _constraintTypes.OfType<NamedTypeSymbol>())
        {
            if (constraint.TypeKind == TypeKind.Interface && seen.Add(constraint))
                builder.Add(constraint);
        }

        return builder.MoveToImmutable();
    }
}

internal sealed class SubstitutedMethodSymbol : MethodSymbol
{
    private readonly MethodSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private readonly Symbol? _associatedSymbol;
    private ImmutableArray<ParameterSymbol>? _parameters;
    private Dictionary<TypeParameterSymbol, ImmutableArray<TypeSymbol>>? _constraintTypeMap;
    private ImmutableArray<MethodSymbol>? _explicitInterfaceImplementations;
    private readonly ImmutableArray<TypeParameterSymbol> _typeParameters;
    private readonly Dictionary<TypeParameterSymbol, TypeParameterSymbol> _methodTypeParameterMap;
    private TypeSymbol? _returnType;

    public SubstitutedMethodSymbol(
        MethodSymbol original,
        ConstructedNamedTypeSymbol constructed,
        Symbol? associatedSymbol = null)
    {
        _original = original;
        _constructed = constructed;
        _associatedSymbol = associatedSymbol;

        if (!_original.TypeParameters.IsDefaultOrEmpty)
        {
            var builder = ImmutableArray.CreateBuilder<TypeParameterSymbol>(_original.TypeParameters.Length);
            _methodTypeParameterMap = new Dictionary<TypeParameterSymbol, TypeParameterSymbol>(
                _original.TypeParameters.Length,
                TypeParameterSubstitutionComparer.Instance);

            foreach (var typeParameter in _original.TypeParameters)
            {
                var substituted = new SubstitutedMethodTypeParameterSymbol(
                    typeParameter,
                    this,
                    _constructed,
                    _constructed.ContainingNamespace ?? typeParameter.ContainingNamespace);

                builder.Add(substituted);
                _methodTypeParameterMap[typeParameter] = substituted;
            }

            _typeParameters = builder.ToImmutable();
        }
        else
        {
            _typeParameters = _original.TypeParameters.IsDefault
                ? ImmutableArray<TypeParameterSymbol>.Empty
                : _original.TypeParameters;
            _methodTypeParameterMap = new Dictionary<TypeParameterSymbol, TypeParameterSymbol>(
                TypeParameterSubstitutionComparer.Instance);
        }
    }

    public TypeSymbol? DeclaredFunctionType => _original.DeclaredFunctionType is { } type ? _constructed.Substitute(type, _methodTypeParameterMap) : null;
    public string Name => _original.Name;
    public TypeSymbol ReturnType => _returnType ??= _constructed.ReanchorNestedTypeIfNeeded(
        _constructed.Substitute(_original.ReturnType, _methodTypeParameterMap),
        _methodTypeParameterMap);

    public ImmutableArray<ParameterSymbol> Parameters =>
        _parameters ??= DeclaredFunctionType is StructuralFunctionTypeSymbol shape
            ? shape.ParameterTypes.Select((type, index) => (ParameterSymbol)new SourceParameterSymbol(
                $"arg{index}", type, this, ContainingType, ContainingNamespace, [], [], shape.ParameterRefKinds[index])).ToImmutableArray()
            : _original.Parameters
            .Select(p => (ParameterSymbol)new SubstitutedParameterSymbol(
                p,
                _constructed,
                _methodTypeParameterMap,
                this))
            .ToImmutableArray();

    public Symbol ContainingSymbol => _constructed;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => _original.GetReturnTypeAttributes();

    public MethodKind MethodKind => _original.MethodKind;
    public bool IsConstructor => _original.IsConstructor;
    public MethodSymbol? OriginalDefinition => _original;
    public bool IsAbstract => _original.IsAbstract;
    public bool IsAsync => _original.IsAsync;
    public bool IsCheckedBuiltin => _original.IsCheckedBuiltin;
    public bool IsDefinition => _original.IsDefinition;
    public bool IsExtensionMethod => _original.IsExtensionMethod;
    public bool IsExtern => _original.IsExtern;
    public bool IsUnsafe => _original.IsUnsafe;
    public bool IsGenericMethod => _original.IsGenericMethod;
    public bool IsOverride => _original.IsOverride;
    public bool IsReadOnly => _original.IsReadOnly;
    public bool IsFinal => _original.IsFinal;
    public bool IsVirtual => _original.IsVirtual;
    public bool SetsRequiredMembers => _original.SetsRequiredMembers;
    public bool IsIterator => _original.IsIterator;
    public IteratorMethodKind IteratorKind => _original.IteratorKind;
    public TypeSymbol? IteratorElementType => _original.IteratorElementType;

    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var originals = _original.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitInterfaceImplementations = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<MethodSymbol>(originals.Length);

            foreach (var origImpl in originals)
            {
                var origIface = (NamedTypeSymbol?)origImpl.ContainingType;
                if (origIface is null)
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Substitute the interface type with the constructed type’s substitution
                var substitutedIface = _constructed.Substitute(origIface) as NamedTypeSymbol;

                // If nothing changed, just reuse the original method
                if (substitutedIface is null ||
                    SymbolEqualityComparer.Default.Equals(origIface, substitutedIface))
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Find the corresponding method on the substituted interface
                MethodSymbol? substitutedMethod = null;

                foreach (var candidate in substitutedIface.GetMembers(origImpl.Name).OfType<MethodSymbol>())
                {
                    // Match via OriginalDefinition – this is the key
                    if (SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition, origImpl.OriginalDefinition))
                    {
                        substitutedMethod = candidate;
                        break;
                    }
                }

                builder.Add(substitutedMethod ?? origImpl);
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    public ImmutableArray<TypeParameterSymbol> TypeParameters => _typeParameters;
    public ImmutableArray<TypeSymbol> TypeArguments => _original.TypeArguments;
    public MethodSymbol? ConstructedFrom => _original.ConstructedFrom ?? _original;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public Symbol? AssociatedSymbol => _associatedSymbol ?? _original.AssociatedSymbol;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public void Accept(SymbolVisitor visitor)
    {
        visitor.VisitMethod(this);
    }

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.VisitMethod(this);
    }

    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) =>
               comparer.Equals(this, other);

    public bool Equals(Symbol? other) =>
        SymbolEqualityComparer.Default.Equals(this, other);

    public MethodSymbol Construct(params TypeSymbol[] typeArguments)
    {
        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray(), _constructed);
    }

    internal bool TryGetSubstitutedConstraintTypes(
        TypeParameterSymbol typeParameter,
        out ImmutableArray<TypeSymbol> substituted)
    {
        substituted = default;

        if (_typeParameters.IsDefaultOrEmpty ||
            !_typeParameters.Any(tp => TypeParameterSubstitutionComparer.Instance.Equals(tp, typeParameter)))
        {
            return false;
        }

        _constraintTypeMap ??= new Dictionary<TypeParameterSymbol, ImmutableArray<TypeSymbol>>(
            TypeParameterSubstitutionComparer.Instance);

        if (_constraintTypeMap.TryGetValue(typeParameter, out substituted))
            return true;

        var originalConstraints = typeParameter.ConstraintTypes;

        if (originalConstraints.IsDefaultOrEmpty || originalConstraints.Length == 0)
        {
            substituted = originalConstraints;
            _constraintTypeMap[typeParameter] = substituted;
            return true;
        }

        var builder = ImmutableArray.CreateBuilder<TypeSymbol>(originalConstraints.Length);

        foreach (var constraint in originalConstraints)
            builder.Add(_constructed.Substitute(constraint, _methodTypeParameterMap));

        substituted = builder.ToImmutable();
        _constraintTypeMap[typeParameter] = substituted;
        return true;
    }

    private string GetDebuggerDisplay()
    {
        try
        {
            return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}";
        }
        catch (Exception exc)
        {
            return $"{Kind}: <{exc.GetType().Name}>";
        }
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }
}

internal sealed class SubstitutedMethodTypeParameterSymbol : TypeParameterSymbol
{
    private readonly TypeParameterSymbol _original;
    private readonly SubstitutedMethodSymbol _containingMethod;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private readonly NamespaceSymbol? _containingNamespace;
    private ImmutableArray<TypeSymbol>? _constraintTypes;

    public SubstitutedMethodTypeParameterSymbol(
        TypeParameterSymbol original,
        SubstitutedMethodSymbol containingMethod,
        ConstructedNamedTypeSymbol constructed,
        NamespaceSymbol? containingNamespace)
    {
        _original = original;
        _containingMethod = containingMethod;
        _constructed = constructed;
        _containingNamespace = containingNamespace;
    }

    public string Name => _original.Name;
    public string MetadataName => _original.MetadataName;
    public SymbolKind Kind => _original.Kind;
    public Symbol? ContainingSymbol => _containingMethod;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _containingNamespace ?? _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => false;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public int Ordinal => _original.Ordinal;
    public TypeParameterOwnerKind OwnerKind => TypeParameterOwnerKind.Method;
    public NamedTypeSymbol? DeclaringTypeParameterOwner => null;
    public MethodSymbol? DeclaringMethodParameterOwner => _containingMethod;
    public TypeParameterConstraintKind ConstraintKind => _original.ConstraintKind;
    public ImmutableArray<TypeSymbol> ConstraintTypes
    {
        get
        {
            if (_constraintTypes.HasValue)
                return _constraintTypes.Value;

            if (_containingMethod.TryGetSubstitutedConstraintTypes(_original, out var substituted))
                _constraintTypes = substituted;
            else
                _constraintTypes = _original.ConstraintTypes;

            return _constraintTypes.Value;
        }
    }
    public VarianceKind Variance => _original.Variance;

    public NamedTypeSymbol? BaseType => _original.BaseType;
    public TypeSymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public SpecialType SpecialType => _original.SpecialType;
    public TypeKind TypeKind => _original.TypeKind;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => _original.IsReferenceType;
    public bool IsValueType => _original.IsValueType;
    public bool IsInterface => _original.IsInterface;
    public bool IsTupleType => _original.IsTupleType;
    public bool IsUnion => _original.IsUnion;
    public bool IsUnionCase => _original.IsUnionCase;
    public NamedTypeSymbol? UnderlyingUnionType => _original.UnderlyingUnionType;
    public ImmutableArray<NamedTypeSymbol> Interfaces => _original.Interfaces;
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => _original.AllInterfaces;
    public ImmutableArray<Symbol> GetMembers() => _original.GetMembers();
    public ImmutableArray<Symbol> GetMembers(string name) => _original.GetMembers(name);
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedFieldSymbol : FieldSymbol
{
    internal FieldSymbol OriginalField => _original;
    private readonly FieldSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private TypeSymbol? _type;

    public SubstitutedFieldSymbol(FieldSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public TypeSymbol Type => _type ??= _constructed.Substitute(_original.Type);
    public Symbol ContainingSymbol => _constructed;

    public bool IsConst => _original.IsConst;
    public bool IsRequired => _original.IsRequired;
    public bool IsReadOnly => _original.IsReadOnly;
    public RefKind RefKind => _original.RefKind;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    internal FieldSymbol OriginalDefinition => _original;

    public object? GetConstantValue() => _original.GetConstantValue();


}

internal sealed class SubstitutedPropertySymbol : PropertySymbol
{
    private readonly PropertySymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private ImmutableArray<PropertySymbol>? _explicitInterfaceImplementations;
    private TypeSymbol? _type;
    private MethodSymbol? _getMethod;
    private MethodSymbol? _setMethod;

    public SubstitutedPropertySymbol(PropertySymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public TypeSymbol Type => _type ??= _constructed.Substitute(_original.Type);
    public Symbol ContainingSymbol => _constructed;
    public PropertySymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public MethodSymbol? GetMethod => _original.GetMethod is null
        ? null
        : _getMethod ??= new SubstitutedMethodSymbol(_original.GetMethod, _constructed, this);
    public MethodSymbol? SetMethod => _original.SetMethod is null
        ? null
        : _setMethod ??= new SubstitutedMethodSymbol(_original.SetMethod, _constructed, this);
    public bool IsIndexer => _original.IsIndexer;
    public bool IsRequired => _original.IsRequired;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var originals = _original.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitInterfaceImplementations = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<PropertySymbol>(originals.Length);

            foreach (var origImpl in originals)
            {
                var origIface = (NamedTypeSymbol?)origImpl.ContainingType;
                if (origIface is null)
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Substitute the interface type with the constructed type’s substitution
                var substitutedIface = _constructed.Substitute(origIface) as NamedTypeSymbol;

                // If nothing changed, just reuse the original property
                if (substitutedIface is null ||
                    SymbolEqualityComparer.Default.Equals(origIface, substitutedIface))
                {
                    builder.Add(origImpl);
                    continue;
                }

                // Find the corresponding property on the substituted interface
                PropertySymbol? substitutedProperty = null;

                foreach (var candidate in substitutedIface.GetMembers(origImpl.Name).OfType<PropertySymbol>())
                {
                    // Match via OriginalDefinition – this is the key
                    if (SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition, origImpl.OriginalDefinition))
                    {
                        substitutedProperty = candidate;
                        break;
                    }
                }

                builder.Add(substitutedProperty ?? origImpl);
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedEventSymbol : EventSymbol
{
    private readonly EventSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private ImmutableArray<EventSymbol>? _explicitInterfaceImplementations;
    private MethodSymbol? _addMethod;
    private MethodSymbol? _removeMethod;

    public SubstitutedEventSymbol(EventSymbol original, ConstructedNamedTypeSymbol constructed)
    {
        _original = original;
        _constructed = constructed;
    }

    public string Name => _original.Name;
    public TypeSymbol Type => _constructed.Substitute(_original.Type);
    public Symbol ContainingSymbol => _constructed;
    public MethodSymbol? AddMethod => _original.AddMethod is null
        ? null
        : _addMethod ??= new SubstitutedMethodSymbol(_original.AddMethod, _constructed, this);
    public MethodSymbol? RemoveMethod => _original.RemoveMethod is null
        ? null
        : _removeMethod ??= new SubstitutedMethodSymbol(_original.RemoveMethod, _constructed, this);
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public ImmutableArray<EventSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var originals = _original.ExplicitInterfaceImplementations;

            if (originals.IsDefaultOrEmpty || originals.Length == 0)
            {
                _explicitInterfaceImplementations = originals;
                return originals;
            }

            var builder = ImmutableArray.CreateBuilder<EventSymbol>(originals.Length);

            foreach (var originalImplementation in originals)
            {
                var originalInterfaceType = originalImplementation.ContainingType;
                if (originalInterfaceType is null)
                {
                    builder.Add(originalImplementation);
                    continue;
                }

                var substitutedInterfaceType = _constructed.Substitute(originalInterfaceType) as NamedTypeSymbol;
                if (substitutedInterfaceType is null ||
                    SymbolEqualityComparer.Default.Equals(originalInterfaceType, substitutedInterfaceType))
                {
                    builder.Add(originalImplementation);
                    continue;
                }

                EventSymbol? substitutedEvent = null;
                foreach (var candidate in substitutedInterfaceType.GetMembers(originalImplementation.Name).OfType<EventSymbol>())
                {
                    if (!SymbolEqualityComparer.Default.Equals(candidate.Type, originalImplementation.Type))
                        continue;

                    substitutedEvent = candidate;
                    break;
                }

                builder.Add(substitutedEvent ?? originalImplementation);
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    public void Accept(SymbolVisitor visitor) => visitor.VisitEvent(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitEvent(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}

internal sealed class SubstitutedParameterSymbol : ParameterSymbol, IParameterDefaultValueInfo
{
    private readonly ParameterSymbol _original;
    private readonly ConstructedNamedTypeSymbol _constructed;
    private readonly Dictionary<TypeParameterSymbol, TypeParameterSymbol>? _methodMap;
    private readonly Symbol _containingSymbol;
    private TypeSymbol? _type;

    public SubstitutedParameterSymbol(
        ParameterSymbol original,
        ConstructedNamedTypeSymbol constructed,
        Dictionary<TypeParameterSymbol, TypeParameterSymbol>? methodMap = null,
        Symbol? containingSymbol = null)
    {
        _original = original;
        _constructed = constructed;
        _methodMap = methodMap;
        _containingSymbol = containingSymbol ?? constructed;
    }

    public string Name => _original.Name;
    public TypeSymbol Type => _type ??= _constructed.ReanchorNestedTypeIfNeeded(
        _constructed.Substitute(_original.Type, _methodMap),
        _methodMap);
    public bool HasImplicitName => _original.HasImplicitName;

    public Syntax.PatternSyntax? BindingPattern => _original.BindingPattern;

    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public Symbol? ContainingSymbol => _containingSymbol;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _constructed;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => false;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public bool IsVarParams => _original.IsVarParams;
    public RefKind RefKind => _original.RefKind;
    public ScopedKind ScopedKind => _original.ScopedKind;
    public bool IsMutable => _original.IsMutable;
    public bool HasExplicitDefaultValue => _original.HasExplicitDefaultValue;
    public object? ExplicitDefaultValue => _original.ExplicitDefaultValue;
    bool IParameterDefaultValueInfo.ExplicitDefaultValueIsTypeDefault
        => _original is IParameterDefaultValueInfo { ExplicitDefaultValueIsTypeDefault: true };

    public void Accept(SymbolVisitor visitor) => visitor.VisitParameter(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitParameter(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);

    private string GetDebuggerDisplay()
    {
        try
        {
            return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}";
        }
        catch (Exception exc)
        {
            return $"{Kind}: <{exc.GetType().Name}>";
        }
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }
}

internal sealed class TypeParameterSubstitutionComparer : IEqualityComparer<TypeParameterSymbol>
{
    public static readonly TypeParameterSubstitutionComparer Instance = new();

    private TypeParameterSubstitutionComparer() { }

    public bool Equals(TypeParameterSymbol? defX, TypeParameterSymbol? defY)
    {
        if (defX is null || defY is null)
            return defX is null && defY is null;

        return defX.Ordinal == defY.Ordinal
            && defX.Name == defY.Name
            && defX.OwnerKind == defY.OwnerKind;
    }

    public int GetHashCode(TypeParameterSymbol def)
    {
        // Avoid recursive symbol-display hashing for constructed/nested contexts.
        return HashCode.Combine(
            def.Name,
            def.Ordinal,
            (int)def.OwnerKind);
    }
}
