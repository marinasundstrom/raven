using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private ImmutableArray<IMethodSymbol> _extensionConversionOperators;
    private bool _extensionConversionOperatorsInitialized;
    private readonly Dictionary<ConversionCacheKey, Conversion> _conversionCache = new(new ConversionCacheKeyComparer());

    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined = true)
    {
        if (source is null || destination is null)
            return Conversion.None;

        var key = new ConversionCacheKey(source, destination, includeUserDefined);
        if (_conversionCache.TryGetValue(key, out var cached))
            return cached;

        var conversion = ClassifyConversionCore(source, destination, includeUserDefined);
        _conversionCache[key] = conversion;
        return conversion;
    }

    private Conversion ClassifyConversionCore(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined = true)
    {
        if (source is null || destination is null)
            return Conversion.None;

        static ITypeSymbol Unalias(ITypeSymbol type, ref bool wasAlias)
        {
            while (true)
            {
                if (!type.IsAlias)
                    return type;

                wasAlias = true;

                if (type.UnderlyingSymbol is ITypeSymbol aliasTarget)
                {
                    type = aliasTarget;
                    continue;
                }

                return type;
            }
        }

        bool sourceUsedAlias = false;
        source = Unalias(source, ref sourceUsedAlias);

        bool destinationUsedAlias = false;
        destination = Unalias(destination, ref destinationUsedAlias);

        if (destination is null)
            return Conversion.None;

        var aliasInvolved = sourceUsedAlias || destinationUsedAlias;

        if (source.SpecialType is SpecialType.System_Unit &&
            destination.SpecialType is SpecialType.System_Void ||
            source.SpecialType is SpecialType.System_Void &&
            destination.SpecialType is SpecialType.System_Unit)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        Conversion Finalize(Conversion conversion)
        {
            if (!conversion.Exists)
                return conversion;

            return conversion.WithAlias(aliasInvolved);
        }

        if (source.ContainsErrorType() || destination.ContainsErrorType())
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));

        if (source is LiteralTypeSymbol litSrc && destination is LiteralTypeSymbol litDest)
            return Equals(litSrc.ConstantValue, litDest.ConstantValue)
                ? Finalize(new Conversion(isImplicit: true, isIdentity: true))
                : Conversion.None;

        if (destination is LiteralTypeSymbol)
            return Conversion.None;

        // Implicit constant numeric conversions (C#-style):
        // An integral constant expression of type int can be implicitly converted to certain smaller integral types
        // (e.g. byte/char) if the value is within the target type's range.
        if (source is LiteralTypeSymbol literalSource)
        {
            // Unwrap literal sources to their underlying numeric type.
            var underlying = literalSource.UnderlyingType;
            if (underlying.SpecialType == SpecialType.System_Int32)
            {
                if (TryGetInt32ConstantValue(literalSource.ConstantValue, out var intValue) &&
                    IsImplicitInt32ConstantConversion(intValue, destination.SpecialType))
                {
                    return Finalize(new Conversion(isImplicit: true, isNumeric: true));
                }
            }
        }

        if (source is INamedTypeSymbol { OriginalDefinition: { } taskSourceDefinition } taskSourceNamed &&
            taskSourceNamed.TypeKind != TypeKind.Error &&
            taskSourceDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            taskSourceNamed.TypeArguments.Length == 1 &&
            taskSourceNamed.TypeArguments[0] is { } taskSourceArgument &&
            taskSourceArgument.SpecialType == SpecialType.System_Unit &&
            destination.SpecialType == SpecialType.System_Threading_Tasks_Task)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        if (destination is INamedTypeSymbol { OriginalDefinition: { } taskDestinationDefinition } taskDestinationNamed &&
            taskDestinationNamed.TypeKind != TypeKind.Error &&
            taskDestinationDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            taskDestinationNamed.TypeArguments.Length == 1 &&
            taskDestinationNamed.TypeArguments[0] is { } taskDestinationArgument &&
            taskDestinationArgument.SpecialType == SpecialType.System_Unit &&
            source.SpecialType == SpecialType.System_Threading_Tasks_Task)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        var sourceUnionCase = source.TryGetDiscriminatedUnionCase();
        var destinationUnion = destination.TryGetDiscriminatedUnion();
        if (sourceUnionCase is not null &&
            destinationUnion is not null &&
            (SymbolEqualityComparer.Default.Equals(sourceUnionCase.Union, destinationUnion) ||
             SymbolEqualityComparer.Default.Equals(sourceUnionCase.Union.OriginalDefinition, destinationUnion.OriginalDefinition)))
        {
            var conversionMethod = FindDiscriminatedUnionConversionMethod(source, destination);
            return Finalize(new Conversion(
                isImplicit: true,
                isDiscriminatedUnion: true,
                isUserDefined: conversionMethod is not null,
                methodSymbol: conversionMethod));
        }

        bool ElementTypesAreCompatible(ITypeSymbol sourceElement, ITypeSymbol destinationElement)
        {
            bool sourceElementUsedAlias = false;
            var unaliasedSource = Unalias(sourceElement, ref sourceElementUsedAlias);

            bool destinationElementUsedAlias = false;
            var unaliasedDestination = Unalias(destinationElement, ref destinationElementUsedAlias);

            if (!unaliasedSource.MetadataIdentityEquals(unaliasedDestination))
                return false;

            if (sourceElementUsedAlias || destinationElementUsedAlias)
                aliasInvolved = true;

            return true;
        }

        if (source is IAddressTypeSymbol addressSource)
        {
            if (destination is ByRefTypeSymbol byRefDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, byRefDestination.ElementType))
            {
                return Finalize(new Conversion(isImplicit: true));
            }

            if (destination is IPointerTypeSymbol addressPointerDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, addressPointerDestination.PointedAtType))
            {
                return Finalize(new Conversion(isImplicit: true, isPointer: true));
            }
        }

        if (source is ByRefTypeSymbol byRefSource &&
            destination is IPointerTypeSymbol pointerDestination &&
            ElementTypesAreCompatible(byRefSource.ElementType, pointerDestination.PointedAtType))
        {
            return Finalize(new Conversion(isImplicit: true, isPointer: true));
        }

        if (source.MetadataIdentityEquals(destination) &&
            !Conversion.IsNullable(source) &&
            !Conversion.IsNullable(destination))
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        static bool UnionContainsNull(ITypeUnionSymbol union)
        {
            foreach (var member in union.Types)
            {
                if (member.TypeKind == TypeKind.Null)
                    return true;

                if (member is ITypeUnionSymbol nested && UnionContainsNull(nested))
                    return true;
            }

            return false;
        }

        if (source.TypeKind == TypeKind.Null)
        {
            if (destination.TypeKind == TypeKind.Nullable)
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            if (destination is ITypeUnionSymbol unionDest && UnionContainsNull(unionDest))
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            return Conversion.None;
        }

        if (source is NullableTypeSymbol nullableSource)
        {
            if (!nullableSource.UnderlyingType.IsValueType)
            {
                var conv = ClassifyConversion(nullableSource.UnderlyingType, destination, includeUserDefined);
                if (conv.Exists)
                    return Finalize(conv);
            }
            else
            {
                if (destination is NullableTypeSymbol nullableDestination && nullableDestination.UnderlyingType.IsValueType)
                {
                    var conv2 = ClassifyConversion(nullableSource.UnderlyingType, nullableDestination.UnderlyingType, includeUserDefined);
                    if (conv2.Exists)
                    {
                        return Finalize(new Conversion(
                            isImplicit: conv2.IsImplicit,
                            isIdentity: conv2.IsIdentity,
                            isNumeric: conv2.IsNumeric,
                            isReference: conv2.IsReference,
                            isBoxing: conv2.IsBoxing,
                            isUnboxing: conv2.IsUnboxing,
                            isUserDefined: conv2.IsUserDefined,
                            isAlias: conv2.IsAlias,
                            isLifted: true,
                            methodSymbol: conv2.MethodSymbol));
                    }
                }

                var conv3 = ClassifyConversion(nullableSource.UnderlyingType, destination, includeUserDefined);
                if (conv3.Exists)
                {
                    var isImplicit = !destination.IsValueType && conv3.IsImplicit;
                    return Finalize(new Conversion(
                        isImplicit: isImplicit,
                        isIdentity: conv3.IsIdentity,
                        isNumeric: conv3.IsNumeric,
                        isReference: conv3.IsReference,
                        isBoxing: conv3.IsBoxing,
                        isUnboxing: conv3.IsUnboxing,
                        isUserDefined: conv3.IsUserDefined,
                        isAlias: conv3.IsAlias,
                        isLifted: true,
                        methodSymbol: conv3.MethodSymbol));
                }
            }
        }

        if (destination is NullableTypeSymbol nullableDest)
        {
            if (!nullableDest.UnderlyingType.IsValueType)
            {
                var conv = ClassifyConversion(source, nullableDest.UnderlyingType, includeUserDefined);
                if (conv.Exists)
                    return Finalize(conv);
            }
            else
            {
                if (source is ITypeUnionSymbol unionSource &&
                    unionSource.Types.Count() == 2 &&
                    unionSource.Types.Any(t => t.TypeKind == TypeKind.Null) &&
                    unionSource.Types.Any(t => t.MetadataIdentityEquals(nullableDest.UnderlyingType)))
                {
                    return Finalize(new Conversion(isImplicit: true, isReference: true));
                }

                var conv = ClassifyConversion(source, nullableDest.UnderlyingType, includeUserDefined);
                if (conv.Exists)
                {
                    return Finalize(new Conversion(
                        isImplicit: true,
                        isIdentity: false,
                        isNumeric: conv.IsNumeric,
                        isReference: conv.IsReference || !source.IsValueType,
                        isBoxing: conv.IsBoxing,
                        isUnboxing: conv.IsUnboxing,
                        isUserDefined: conv.IsUserDefined,
                        isAlias: conv.IsAlias,
                        isLifted: true,
                        methodSymbol: conv.MethodSymbol));
                }
            }
        }

        if (source is ITypeUnionSymbol unionSource2)
        {
            var conversions = unionSource2.Types.Select(t => ClassifyConversion(t, destination, includeUserDefined)).ToArray();
            if (conversions.All(c => c.Exists))
            {
                var isImplicit = conversions.All(c => c.IsImplicit);
                var isAlias = conversions.Any(c => c.IsAlias);
                var destinationIsValueType = destination.IsValueType;

                return Finalize(new Conversion(
                    isImplicit: isImplicit,
                    isReference: !destinationIsValueType,
                    isUnboxing: destinationIsValueType,
                    isAlias: isAlias));
            }

            return Conversion.None;
        }

        if (source.SpecialType == SpecialType.System_Void)
            return Conversion.None;

        var objType = GetSpecialType(SpecialType.System_Object);

        if (destination.MetadataIdentityEquals(objType))
        {
            if (source.MetadataIdentityEquals(objType))
            {
                return Conversion.None;
            }

            return Finalize(new Conversion(
                isImplicit: true,
                isReference: !source.IsValueType,
                isBoxing: source.IsValueType));
        }

        if (destination is ITypeUnionSymbol unionType)
        {
            Conversion matchConversion = default;
            var foundMatch = false;

            foreach (var branch in unionType.Types)
            {
                var branchConversion = ClassifyConversion(source, branch, includeUserDefined);
                if (branchConversion.Exists)
                {
                    matchConversion = branchConversion;
                    foundMatch = true;
                    break;
                }
            }

            if (!foundMatch)
                return Conversion.None;

            return Finalize(new Conversion(
                isImplicit: true,
                isBoxing: source.IsValueType,
                isAlias: matchConversion.IsAlias));
        }

        if (source is LiteralTypeSymbol litSrc2)
            return Finalize(ClassifyConversion(litSrc2.UnderlyingType, destination, includeUserDefined));

        if (IsReferenceConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isReference: true));
        }

        if (IsExplicitReferenceConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isReference: true));
        }

        if (IsBoxingConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isBoxing: true));
        }

        if (IsUnboxingConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isUnboxing: true));
        }

        // Explicit enum -> underlying enum type conversion.
        // Example: `val x: E = ...; val i: int = (int)x`.
        // Treat as an explicit numeric conversion.
        if (source is INamedTypeSymbol sourceNamedEnum &&
            sourceNamedEnum.TypeKind == TypeKind.Enum &&
            sourceNamedEnum.EnumUnderlyingType is { } enumUnderlyingType &&
            destination.MetadataIdentityEquals(enumUnderlyingType))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        if (destination is INamedTypeSymbol destNamedEnum &&
            destNamedEnum.TypeKind == TypeKind.Enum &&
            destNamedEnum.EnumUnderlyingType is { } enumUnderlyingType2 &&
            source.MetadataIdentityEquals(enumUnderlyingType2))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        if (IsImplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isNumeric: true));
        }

        if (IsExplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        var sourceNamed = source as INamedTypeSymbol;
        var destinationNamed = destination as INamedTypeSymbol;

        if (includeUserDefined && (sourceNamed != null || destinationNamed != null))
        {
            IEnumerable<IMethodSymbol> candidateConversions =
                Enumerable.Empty<IMethodSymbol>();

            if (sourceNamed != null)
                candidateConversions = candidateConversions.Concat(sourceNamed.GetMembers().OfType<IMethodSymbol>());
            if (destinationNamed != null && !source.MetadataIdentityEquals(destination))
                candidateConversions = candidateConversions.Concat(destinationNamed.GetMembers().OfType<IMethodSymbol>());

            candidateConversions = candidateConversions.Concat(GetExtensionConversionCandidates(source, destination));

            foreach (var method in candidateConversions)
            {
                if (method.MethodKind is not MethodKind.Conversion ||
                    method.Parameters.Length != 1)
                {
                    continue;
                }

                var sourceConversion = ClassifyConversion(source, method.Parameters[0].Type, includeUserDefined: false);
                var sourceConvertible = sourceConversion.Exists && sourceConversion.IsImplicit;

                if (!sourceConvertible && SourceMatchesDiscriminatedUnionCase(source, method.Parameters[0].Type))
                    sourceConvertible = true;

                if (sourceConvertible &&
                    ClassifyConversion(method.ReturnType, destination, includeUserDefined: false) is { Exists: true, IsImplicit: true })
                {
                    var isImplicit = method.Name == "op_Implicit";
                    return Finalize(new Conversion(isImplicit: isImplicit, isUserDefined: true, methodSymbol: method));
                }
            }
        }

        if (source.TryGetDiscriminatedUnionCase() is { } unionCase &&
            !SymbolEqualityComparer.Default.Equals(source, unionCase.Union))
        {
            var unionConversion = ClassifyConversion(unionCase.Union, destination, includeUserDefined);
            if (unionConversion.Exists)
                return unionConversion;
        }

        return Conversion.None;

        static bool TryGetInt32ConstantValue(object? constantValue, out int value)
        {
            switch (constantValue)
            {
                case int i:
                    value = i;
                    return true;

                case byte b:
                    value = b;
                    return true;

                case char c:
                    value = c;
                    return true;

                case sbyte sb:
                    value = sb;
                    return true;

                case short s:
                    value = s;
                    return true;

                case ushort us:
                    value = us;
                    return true;

                default:
                    value = default;
                    return false;
            }
        }

        static bool IsImplicitInt32ConstantConversion(int value, SpecialType destination)
        {
            // Match C#'s implicit constant expression conversions for the subset Raven currently supports.
            // Today we only need byte/char, but keeping this structured makes it easy to extend.
            return destination switch
            {
                SpecialType.System_Byte => (uint)value <= byte.MaxValue,
                SpecialType.System_Char => (uint)value <= char.MaxValue,
                _ => false
            };
        }

        static bool SourceMatchesDiscriminatedUnionCase(ITypeSymbol sourceType, ITypeSymbol parameterType)
        {
            var sourceCase = sourceType.TryGetDiscriminatedUnionCase();
            var parameterUnion = parameterType.TryGetDiscriminatedUnion();

            if (sourceCase is null || parameterUnion is null)
                return false;

            return SymbolEqualityComparer.Default.Equals(sourceCase.Union, parameterUnion) ||
                SymbolEqualityComparer.Default.Equals(sourceCase.Union.OriginalDefinition, parameterUnion.OriginalDefinition);
        }
    }

    private readonly record struct ConversionCacheKey(
        ITypeSymbol Source,
        ITypeSymbol Destination,
        bool IncludeUserDefined);

    private sealed class ConversionCacheKeyComparer : IEqualityComparer<ConversionCacheKey>
    {
        public bool Equals(ConversionCacheKey x, ConversionCacheKey y)
        {
            return x.IncludeUserDefined == y.IncludeUserDefined &&
                SymbolEqualityComparer.Default.Equals(x.Source, y.Source) &&
                SymbolEqualityComparer.Default.Equals(x.Destination, y.Destination);
        }

        public int GetHashCode(ConversionCacheKey obj)
        {
            return HashCode.Combine(
                SymbolEqualityComparer.Default.GetHashCode(obj.Source),
                SymbolEqualityComparer.Default.GetHashCode(obj.Destination),
                obj.IncludeUserDefined);
        }
    }

    private IEnumerable<IMethodSymbol> GetExtensionConversionCandidates(ITypeSymbol source, ITypeSymbol destination)
    {
        var extensionOperators = GetExtensionConversionOperators();
        if (extensionOperators.IsDefaultOrEmpty)
            yield break;

        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var receiverTypes = ImmutableArray.CreateBuilder<ITypeSymbol>(4);
        receiverTypes.Add(source);
        receiverTypes.Add(destination);

        if (source.TryGetDiscriminatedUnionCase() is { } sourceUnionCase)
            receiverTypes.Add(sourceUnionCase.Union);
        if (destination.TryGetDiscriminatedUnionCase() is { } destinationUnionCase)
            receiverTypes.Add(destinationUnionCase.Union);

        foreach (var method in extensionOperators)
        {
            IMethodSymbol? constructed = null;

            foreach (var receiverType in receiverTypes)
            {
                constructed = TryConstructExtensionConversion(method, receiverType);
                if (constructed is not null)
                    break;
            }

            constructed ??= method;

            if (constructed is not null && seen.Add(constructed))
                yield return constructed;
        }
    }

    private ImmutableArray<IMethodSymbol> GetExtensionConversionOperators()
    {
        if (_extensionConversionOperatorsInitialized)
            return _extensionConversionOperators;

        _extensionConversionOperatorsInitialized = true;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();
        var members = GlobalNamespace.GetAllMembersRecursive().OfType<INamedTypeSymbol>();

        foreach (var type in members)
        {
            if (!type.HasStaticExtensionMembers())
                continue;

            foreach (var member in type.GetMembers("op_Implicit").Concat(type.GetMembers("op_Explicit")))
            {
                if (member is not IMethodSymbol method)
                    continue;

                if (method.MethodKind is not MethodKind.Conversion)
                    continue;

                if (method.GetExtensionReceiverType() is null)
                    continue;

                builder.Add(method);
            }
        }

        _extensionConversionOperators = builder.ToImmutable();
        return _extensionConversionOperators;
    }

    private IMethodSymbol? TryConstructExtensionConversion(IMethodSymbol method, ITypeSymbol receiverType)
    {
        var extensionReceiverType = method.GetExtensionReceiverType();
        if (extensionReceiverType is null)
            return null;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return null;

        var methodDefinition = method.OriginalDefinition ?? method;
        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions))
            return null;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new ITypeSymbol[methodDefinition.TypeParameters.Length];

            for (int i = 0; i < methodDefinition.TypeParameters.Length; i++)
            {
                var typeParameter = methodDefinition.TypeParameters[i];
                if (!TryGetMethodSubstitution(substitutions, typeParameter, out var typeArgument))
                    return null;

                methodTypeArguments[i] = typeArgument;
            }

            if (!SatisfiesTypeParameterConstraints(methodDefinition.TypeParameters, methodTypeArguments))
                return null;

            if (!SatisfiesContainingTypeConstraints(methodDefinition, substitutions))
                return null;

            return methodDefinition.Construct(methodTypeArguments);
        }

        if (methodDefinition.ContainingType is not INamedTypeSymbol container)
            return null;

        var containerDefinition = container.ConstructedFrom as INamedTypeSymbol ?? container;
        if (!containerDefinition.IsGenericType ||
            containerDefinition.TypeParameters.IsDefaultOrEmpty ||
            containerDefinition.TypeParameters.Length == 0)
        {
            return method;
        }

        var typeArguments = new ITypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return null;

            typeArguments[i] = typeArgument;
        }

        if (!SatisfiesTypeParameterConstraints(containerDefinition.TypeParameters, typeArguments))
            return null;

        if (containerDefinition.Construct(typeArguments) is not INamedTypeSymbol constructedContainer)
            return null;

        var originalDefinition = methodDefinition;

        foreach (var candidate in constructedContainer.GetMembers(method.Name).OfType<IMethodSymbol>())
        {
            var candidateOriginal = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateOriginal, originalDefinition))
                return candidate;
        }

        return null;

        static bool TryGetMethodSubstitution(
            Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions,
            ITypeParameterSymbol methodParameter,
            out ITypeSymbol typeArgument)
        {
            if (substitutions.TryGetValue(methodParameter, out typeArgument))
                return true;

            foreach (var (parameter, argument) in substitutions)
            {
                if (string.Equals(parameter.Name, methodParameter.Name, StringComparison.Ordinal) &&
                    parameter.Ordinal == methodParameter.Ordinal)
                {
                    typeArgument = argument;
                    return true;
                }
            }

            typeArgument = null!;
            return false;
        }
    }

    private static bool SatisfiesTypeParameterConstraints(
        ImmutableArray<ITypeParameterSymbol> typeParameters,
        ITypeSymbol[] typeArguments)
    {
        if (typeParameters.Length != typeArguments.Length)
            return false;

        for (int i = 0; i < typeParameters.Length; i++)
        {
            if (!typeArguments[i].SatisfiesConstraints(typeParameters[i]))
                return false;
        }

        return true;
    }

    private static bool SatisfiesContainingTypeConstraints(
        IMethodSymbol methodDefinition,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (methodDefinition.ContainingType is not INamedTypeSymbol containingType)
            return true;

        var containerDefinition = containingType.ConstructedFrom as INamedTypeSymbol ?? containingType;
        if (!containerDefinition.IsGenericType || containerDefinition.TypeParameters.IsDefaultOrEmpty)
            return true;

        var typeArguments = new ITypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        return SatisfiesTypeParameterConstraints(containerDefinition.TypeParameters, typeArguments);
    }

    private bool TryUnifyExtensionReceiver(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        parameterType = NormalizeTypeForExtensionInference(parameterType);
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (parameterType is ITypeParameterSymbol parameter)
            return TryRecordExtensionSubstitution(parameter, argumentType, substitutions);

        if (parameterType is INamedTypeSymbol paramNamed && argumentType is INamedTypeSymbol argNamed)
        {
            if (TryUnifyNamedType(paramNamed, argNamed, substitutions))
                return true;

            // NOTE: Problem seems to occur when iterating all interfaces. 
            // It lazily creates the types. But something is wrong when 
            foreach (var iface in argNamed.AllInterfaces)
            {
                if (TryUnifyNamedType(paramNamed, iface, substitutions))
                    return true;
            }

            for (var baseType = argNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
            {
                if (TryUnifyNamedType(paramNamed, baseType, substitutions))
                    return true;
            }

            return false;
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
            return TryUnifyExtensionReceiver(paramArray.ElementType, argArray.ElementType, substitutions);

        if (parameterType is NullableTypeSymbol paramNullable)
        {
            if (argumentType is NullableTypeSymbol argNullable)
                return TryUnifyExtensionReceiver(paramNullable.UnderlyingType, argNullable.UnderlyingType, substitutions);

            if (!argumentType.IsValueType)
                return TryUnifyExtensionReceiver(paramNullable.UnderlyingType, argumentType, substitutions);

            return false;
        }

        return SymbolEqualityComparer.Default.Equals(parameterType, argumentType);

        bool TryUnifyNamedType(
            INamedTypeSymbol parameterNamed,
            INamedTypeSymbol? argumentNamed,
            Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
        {
            if (argumentNamed is null)
                return false;

            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return false;

            var parameterArguments = parameterNamed.TypeArguments;
            var argumentArguments = argumentNamed.TypeArguments;

            if (parameterArguments.Length != argumentArguments.Length)
                return false;

            for (int i = 0; i < parameterArguments.Length; i++)
            {
                if (!TryUnifyExtensionReceiver(parameterArguments[i], argumentArguments[i], map))
                    return false;
            }

            return true;
        }
    }

    private bool TryRecordExtensionSubstitution(
        ITypeParameterSymbol typeParameter,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (substitutions.TryGetValue(typeParameter, out var existing))
        {
            existing = NormalizeTypeForExtensionInference(existing);

            if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                return true;

            if (ClassifyConversion(argumentType, existing).IsImplicit)
                return true;

            if (ClassifyConversion(existing, argumentType).IsImplicit)
            {
                substitutions[typeParameter] = argumentType;
                return true;
            }

            return false;
        }

        substitutions[typeParameter] = argumentType;
        return true;
    }

    private static ITypeSymbol NormalizeTypeForExtensionInference(ITypeSymbol type)
    {
        return type switch
        {
            LiteralTypeSymbol literal => literal.UnderlyingType,
            _ => type
        };
    }

    private static IMethodSymbol? FindDiscriminatedUnionConversionMethod(ITypeSymbol source, ITypeSymbol destination)
    {
        if (destination is INamedTypeSymbol destinationNamed)
        {
            var method = FindMatchingConversion(destinationNamed, source, destination);
            if (method is not null)
                return method;
        }

        if (source is INamedTypeSymbol sourceNamed)
        {
            var method = FindMatchingConversion(sourceNamed, source, destination);
            if (method is not null)
                return method;
        }

        return null;

        static IMethodSymbol? FindMatchingConversion(
            INamedTypeSymbol owner,
            ITypeSymbol sourceType,
            ITypeSymbol destinationType)
        {
            foreach (var member in owner.GetMembers("op_Implicit"))
            {
                if (member is not IMethodSymbol method)
                    continue;

                if (method.MethodKind is not MethodKind.Conversion)
                    continue;

                if (method.Parameters.Length != 1)
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, sourceType) &&
                    !SymbolEqualityComparer.Default.Equals(
                        method.Parameters[0].Type.OriginalDefinition ?? method.Parameters[0].Type,
                        sourceType.OriginalDefinition ?? sourceType))
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.ReturnType, destinationType) &&
                    !SymbolEqualityComparer.Default.Equals(
                        method.ReturnType.OriginalDefinition ?? method.ReturnType,
                        destinationType.OriginalDefinition ?? destinationType))
                    continue;

                return method;
            }

            return null;
        }
    }

    private bool IsReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        if (source.IsValueType || destination.IsValueType)
            return false;

        if (source.MetadataIdentityEquals(destination))
            return false;

        var current = source.BaseType;
        while (current is not null)
        {
            if (current.MetadataIdentityEquals(destination))
                return true;

            current = current.BaseType;
        }

        if (destination is INamedTypeSymbol destinationNamed &&
            destinationNamed.TypeKind == TypeKind.Interface)
        {
            if (SemanticFacts.ImplementsInterface(source, destinationNamed, SymbolEqualityComparer.Default))
                return true;
        }

        if (source.TypeKind == TypeKind.Interface && destination.SpecialType is SpecialType.System_Object)
            return true;

        return false;
    }

    private bool IsExplicitReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        if (source.IsValueType || destination.IsValueType)
            return false;

        if (source.MetadataIdentityEquals(destination))
            return false;

        var comparer = SymbolEqualityComparer.Default;

        if (SemanticFacts.IsDerivedFrom(destination, source, comparer))
            return true;

        if (source.SpecialType is SpecialType.System_Object && !destination.IsValueType)
            return true;

        if (source is INamedTypeSymbol sourceInterface && sourceInterface.TypeKind == TypeKind.Interface)
        {
            if (destination.SpecialType is SpecialType.System_Object)
                return true;

            if (destination is INamedTypeSymbol destinationNamed &&
                destinationNamed.TypeKind != TypeKind.Interface &&
                SemanticFacts.ImplementsInterface(destinationNamed, sourceInterface, comparer))
            {
                return true;
            }

            if (destination is INamedTypeSymbol sourceToTargetInterface && sourceToTargetInterface.TypeKind == TypeKind.Interface &&
                (SemanticFacts.ImplementsInterface(sourceInterface, sourceToTargetInterface, comparer) ||
                 SemanticFacts.ImplementsInterface(sourceToTargetInterface, sourceInterface, comparer)))
            {
                return true;
            }
        }

        if (destination is INamedTypeSymbol targetInterface && targetInterface.TypeKind == TypeKind.Interface &&
            SemanticFacts.ImplementsInterface(source, targetInterface, comparer))
        {
            return true;
        }

        return false;
    }

    private bool IsBoxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.IsValueType && destination.SpecialType is SpecialType.System_Object;
    }

    private bool IsUnboxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.SpecialType is SpecialType.System_Object && destination.IsValueType;
    }

    private bool IsImplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        // NOTE:
        // - No implicit conversion between decimal and float/double in C#.
        // - C# treats byte and char as numeric for conversion purposes.
        // - We include integral/char -> double and integral/char -> decimal to support binary numeric promotion.

        return (sourceType, destType) switch
        {
            // -------- integral widening --------
            (SpecialType.System_Byte, SpecialType.System_Int32) => true,
            (SpecialType.System_Byte, SpecialType.System_Int64) => true,

            (SpecialType.System_Char, SpecialType.System_Int32) => true,
            (SpecialType.System_Char, SpecialType.System_Int64) => true,

            (SpecialType.System_Int32, SpecialType.System_Int64) => true,

            // -------- to floating --------
            (SpecialType.System_Byte, SpecialType.System_Single) => true,
            (SpecialType.System_Byte, SpecialType.System_Double) => true,

            (SpecialType.System_Char, SpecialType.System_Single) => true,
            (SpecialType.System_Char, SpecialType.System_Double) => true,

            (SpecialType.System_Int32, SpecialType.System_Single) => true,
            (SpecialType.System_Int32, SpecialType.System_Double) => true,
            (SpecialType.System_Int64, SpecialType.System_Single) => true,
            (SpecialType.System_Int64, SpecialType.System_Double) => true,
            (SpecialType.System_Single, SpecialType.System_Double) => true,

            // -------- to decimal (C# allows implicit integral/char -> decimal) --------
            (SpecialType.System_Byte, SpecialType.System_Decimal) => true,
            (SpecialType.System_Char, SpecialType.System_Decimal) => true,
            (SpecialType.System_Int32, SpecialType.System_Decimal) => true,
            (SpecialType.System_Int64, SpecialType.System_Decimal) => true,

            _ => false
        };
    }

    private bool IsExplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType, destType) switch
        {
            // -------- to byte --------
            (SpecialType.System_Int32, SpecialType.System_Byte) => true,
            (SpecialType.System_Int64, SpecialType.System_Byte) => true,
            (SpecialType.System_Char, SpecialType.System_Byte) => true,
            (SpecialType.System_Single, SpecialType.System_Byte) => true,
            (SpecialType.System_Double, SpecialType.System_Byte) => true,
            (SpecialType.System_Decimal, SpecialType.System_Byte) => true,

            // -------- to char --------
            (SpecialType.System_Byte, SpecialType.System_Char) => true,
            (SpecialType.System_Int32, SpecialType.System_Char) => true,
            (SpecialType.System_Int64, SpecialType.System_Char) => true,
            (SpecialType.System_Single, SpecialType.System_Char) => true,
            (SpecialType.System_Double, SpecialType.System_Char) => true,
            (SpecialType.System_Decimal, SpecialType.System_Char) => true,

            // -------- floating to integral --------
            (SpecialType.System_Double, SpecialType.System_Int32) => true,
            (SpecialType.System_Double, SpecialType.System_Int64) => true,
            (SpecialType.System_Single, SpecialType.System_Int32) => true,
            (SpecialType.System_Single, SpecialType.System_Int64) => true,

            // -------- integral narrowing --------
            (SpecialType.System_Int64, SpecialType.System_Int32) => true,

            // -------- decimal to integral --------
            (SpecialType.System_Decimal, SpecialType.System_Int32) => true,
            (SpecialType.System_Decimal, SpecialType.System_Int64) => true,

            // -------- decimal <-> floating are explicit only in C# --------
            (SpecialType.System_Decimal, SpecialType.System_Double) => true,
            (SpecialType.System_Double, SpecialType.System_Decimal) => true,
            (SpecialType.System_Decimal, SpecialType.System_Single) => true,
            (SpecialType.System_Single, SpecialType.System_Decimal) => true,

            // explicit from double to float
            (SpecialType.System_Double, SpecialType.System_Single) => true,

            _ => false
        };
    }
}
