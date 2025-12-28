using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private ImmutableArray<IMethodSymbol> _extensionConversionOperators;
    private bool _extensionConversionOperatorsInitialized;

    private readonly Dictionary<(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined), Conversion>
        _conversionCache = new(new ConversionCacheComparer());

    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined = true)
    {
        if (source is null || destination is null)
            return Conversion.None;

        // Determine alias involvement + normalize for cache key
        bool srcAlias = false, dstAlias = false;
        var srcKey = UnaliasForKey(source, ref srcAlias);
        var dstKey = UnaliasForKey(destination, ref dstAlias);

        var key = (srcKey, dstKey, includeUserDefined);

        if (_conversionCache.TryGetValue(key, out var cached))
            return cached.WithAlias(srcAlias || dstAlias); // if you want alias-accurate per call

        var computed = ClassifyConversionInternal(source, destination, includeUserDefined); // internal does proper alias tracking
        _conversionCache[key] = computed.WithAlias(false); // store canonical, no-alias
        return computed;
    }

    static ITypeSymbol UnaliasForKey(ITypeSymbol type, ref bool wasAlias)
    {
        while (type is { IsAlias: true, UnderlyingSymbol: ITypeSymbol t })
        {
            wasAlias = true;
            type = t;
        }
        return type;
    }

    private Conversion ClassifyConversionInternal(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined)
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
            source is not NullableTypeSymbol &&
            destination is not NullableTypeSymbol)
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
            var conv = ClassifyConversion(nullableSource.UnderlyingType, destination, includeUserDefined);
            if (conv.Exists)
            {
                var isImplicit = !nullableSource.UnderlyingType.MetadataIdentityEquals(destination) && conv.IsImplicit;
                return Finalize(new Conversion(
                    isImplicit: isImplicit,
                    isIdentity: conv.IsIdentity,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined,
                    isAlias: conv.IsAlias,
                    methodSymbol: conv.MethodSymbol));
            }
        }

        if (destination is NullableTypeSymbol nullableDest)
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
                return Finalize(new Conversion(
                    isImplicit: true,
                    isIdentity: false,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference || !source.IsValueType,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined,
                    isAlias: conv.IsAlias,
                    methodSymbol: conv.MethodSymbol));
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

                // ✅ NEW: if method (or containing type) has type parameters, ensure it's a valid constructed method
                if (!IsValidConstructedMethodWithConstraints(method))
                    continue;

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

    private bool IsValidConstructedMethodWithConstraints(IMethodSymbol method)
    {
        // If it’s a definition, it’s not applicable here anyway (needs inference/construct).
        // If your pipeline can surface open generic methods as members, decide whether to skip them.
        if (method.ContainsTypeParameters())
            return false;

        // If you can reach the original definition + applied arguments, validate.
        // This depends on your symbol model. Typical pattern:
        // - method.OriginalDefinition.TypeParameters and method.TypeArguments
        // - method.ContainingType.OriginalDefinition.TypeParameters and method.ContainingType.TypeArguments

        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        if (method.OriginalDefinition is { } def && def.TypeParameters.Length > 0)
        {
            var args = method.TypeArguments;
            for (int i = 0; i < def.TypeParameters.Length; i++)
                substitutions[def.TypeParameters[i]] = args[i];

            if (!SatisfiesAllConstraints(def.TypeParameters, args, substitutions))
                return false;
        }

        if (method.ContainingType is INamedTypeSymbol ct && (ct.OriginalDefinition ?? ct).IsGenericType)
        {
            var ctDef = ct.OriginalDefinition ?? ct;
            var ctArgs = ct.TypeArguments;

            // merge container mappings too
            for (int i = 0; i < ctDef.TypeParameters.Length; i++)
                substitutions[ctDef.TypeParameters[i]] = ctArgs[i];

            if (!SatisfiesAllConstraints(ctDef.TypeParameters, ctArgs, substitutions))
                return false;
        }

        return true;
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

        // Construct method type args (if any)
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

            var methodTypeArgsArray = methodTypeArguments.ToImmutableArray();

            // ✅ NEW: validate method constraints
            if (!SatisfiesAllConstraints(methodDefinition.TypeParameters, methodTypeArgsArray, substitutions))
                return null;

            method = methodDefinition.Construct([.. methodTypeArguments]);
            methodDefinition = method.OriginalDefinition ?? method;
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

        var containerTypeArgsArray = typeArguments.ToImmutableArray();

        // ✅ NEW: validate container constraints too
        if (!SatisfiesAllConstraints(containerDefinition.TypeParameters, containerTypeArgsArray, substitutions))
            return null;

        if (containerDefinition.Construct([.. typeArguments]) is not INamedTypeSymbol constructedContainer)
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

    private bool TryUnifyExtensionReceiver(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        File.WriteAllText("Compilation.debug.txt", $"DEBUG (Compilation.TryUnifyExtensionReceiver). Param type: {parameterType}; Argument type: {argumentType}");

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

                if (!SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, sourceType))
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.ReturnType, destinationType))
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

        return (sourceType is SpecialType.System_Int32 && destType is SpecialType.System_Int64) ||
               (sourceType is SpecialType.System_Int32 && destType is SpecialType.System_Double) ||
               (sourceType is SpecialType.System_Single && destType is SpecialType.System_Double);
    }

    private bool IsExplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType is SpecialType.System_Double && destType is SpecialType.System_Int32) ||
               (sourceType is SpecialType.System_Int64 && destType is SpecialType.System_Int32);
    }

    private bool SatisfiesAllConstraints(
    ImmutableArray<ITypeParameterSymbol> typeParameters,
    ImmutableArray<ITypeSymbol> typeArguments,
    Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        if (typeParameters.Length != typeArguments.Length)
            return false;

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var tp = typeParameters[i];
            var ta = NormalizeTypeForExtensionInference(typeArguments[i]);

            if (!SatisfiesConstraints(tp, ta, substitutions))
                return false;
        }

        return true;
    }

    private bool SatisfiesConstraints(
        ITypeParameterSymbol typeParameter,
        ITypeSymbol typeArgument,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        // Substitute other type parameters inside constraints (e.g., "where T : U")
        ITypeSymbol Subst(ITypeSymbol t) => SubstituteTypeParameters(t, substitutions);

        // 1) Special constraints
        if (typeParameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ReferenceType))
        {
            if (typeArgument.IsValueType)
                return false;
        }

        if (typeParameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ValueType))
        {
            // Usually: must be non-nullable value type
            if (!typeArgument.IsValueType || typeArgument.TypeKind == TypeKind.Nullable)
                return false;
        }

        if (typeParameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.Nullable))
        {
            // If you have nullability annotations in your type system, check them here.
            // Fallback rule: reject literal null / NullType.
            if (typeArgument.TypeKind == TypeKind.Null)
                return false;
        }

        if (typeParameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.ParameterlessConstructor))
        {
            // "new()" constraint: must have public parameterless ctor, and not be abstract.
            // For value types, it's always OK in C#; decide what Raven wants.
            if (!HasAccessibleParameterlessConstructor(typeArgument))
                return false;
        }

        // If you support "unmanaged" or other runtime constraints, validate them here too.

        // 2) Type constraints: "where T : Base, IFoo, ..."
        foreach (var rawConstraint in typeParameter.ConstraintTypes)
        {
            var constraint = Subst(rawConstraint);

            // If constraint still contains unresolved type parameters, inference is incomplete -> fail.
            if (constraint.ContainsTypeParameter())
                return false;

            // Accept if typeArgument converts implicitly to constraint.
            // This matches the usual “T must be assignable to constraint” semantics.
            var conv = ClassifyConversion(typeArgument, constraint, includeUserDefined: false);
            if (!conv.Exists || !conv.IsImplicit)
                return false;
        }

        return true;
    }

    private ITypeSymbol SubstituteTypeParameters(
        ITypeSymbol type,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        type = NormalizeTypeForExtensionInference(type);

        if (type is ITypeParameterSymbol tp && substitutions.TryGetValue(tp, out var mapped))
            return mapped;

        if (type is NullableTypeSymbol nt)
        {
            var inner = SubstituteTypeParameters(nt.UnderlyingType, substitutions);
            return inner.MetadataIdentityEquals(nt.UnderlyingType) ? type : new NullableTypeSymbol(inner);
        }

        if (type is IArrayTypeSymbol arr)
        {
            var elem = SubstituteTypeParameters(arr.ElementType, substitutions);
            return elem.MetadataIdentityEquals(arr.ElementType) ? type : CreateArrayTypeSymbol(elem, arr.Rank);
        }

        if (type is INamedTypeSymbol named && named.IsGenericType)
        {
            var args = named.TypeArguments;
            var changed = false;

            var newArgs = ImmutableArray.CreateBuilder<ITypeSymbol>(args.Length);
            foreach (var a in args)
            {
                var na = SubstituteTypeParameters(a, substitutions);
                changed |= !na.MetadataIdentityEquals(a);
                newArgs.Add(na);
            }

            if (!changed)
                return type;

            // Reconstruct as constructed generic type
            var def = named.OriginalDefinition ?? named;
            return def.Construct(newArgs.ToImmutable());
        }

        return type;
    }

    private bool HasAccessibleParameterlessConstructor(ITypeSymbol type)
    {
        if (type.IsValueType)
            return true; // C#-like rule; change if Raven differs.

        if (type is not INamedTypeSymbol named || named.TypeKind == TypeKind.Interface)
            return false;

        if (named.IsAbstract)
            return false;

        // Look for an instance ctor with 0 params, accessible.
        foreach (var m in named.GetMembers(".ctor").OfType<IMethodSymbol>())
        {
            if (!m.IsStatic && m.Parameters.Length == 0 /* && m.IsAccessibleFrom(/*whatever your accessibility context is) */)
                return true;
        }

        return false;
    }

    private sealed class ConversionCacheComparer
        : IEqualityComparer<(ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined)>
    {
        public bool Equals(
            (ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined) x,
            (ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined) y)
            => x.includeUserDefined == y.includeUserDefined
               && SymbolEqualityComparer.Default.Equals(x.source, y.source)
               && SymbolEqualityComparer.Default.Equals(x.destination, y.destination);

        public int GetHashCode((ITypeSymbol source, ITypeSymbol destination, bool includeUserDefined) obj)
        {
            unchecked
            {
                var h = 17;
                h = h * 31 + SymbolEqualityComparer.Default.GetHashCode(obj.source);
                h = h * 31 + SymbolEqualityComparer.Default.GetHashCode(obj.destination);
                h = h * 31 + obj.includeUserDefined.GetHashCode();
                return h;
            }
        }
    }
}
