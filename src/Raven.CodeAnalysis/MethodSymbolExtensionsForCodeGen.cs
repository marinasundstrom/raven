using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class MethodSymbolExtensionsForCodeGen
{
    internal static MethodInfo GetClrMethodInfo(this IMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        if (methodSymbol is null)
            throw new ArgumentNullException(nameof(methodSymbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        if (codeGen.TryGetRuntimeMethod(methodSymbol, out var cached))
            return cached;

        return methodSymbol switch
        {
            IMethodSymbol wrapper when wrapper.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, wrapper)
                => codeGen.CacheRuntimeMethod(methodSymbol, underlying.GetClrMethodInfo(codeGen)),
            IAliasSymbol aliasSymbol when aliasSymbol.UnderlyingSymbol is IMethodSymbol underlyingMethod
                => codeGen.CacheRuntimeMethod(methodSymbol, underlyingMethod.GetClrMethodInfo(codeGen)),
            SourceMethodSymbol sourceMethod
                => codeGen.CacheRuntimeMethod(methodSymbol, (MethodInfo)codeGen.GetMemberBuilder(sourceMethod)),
            SubstitutedMethodSymbol substitutedMethod
                => codeGen.CacheRuntimeMethod(methodSymbol, substitutedMethod.GetMethodInfo(codeGen)),
            ConstructedMethodSymbol constructedMethod
                => codeGen.CacheRuntimeMethod(methodSymbol, constructedMethod.GetMethodInfo(codeGen)),
            PEMethodSymbol peMethod => codeGen.CacheRuntimeMethod(methodSymbol, ResolveRuntimeMethodInfo(peMethod, codeGen)),
            _ => throw new InvalidOperationException($"Unsupported method symbol type '{methodSymbol.GetType()}'.")
        };
    }

    internal static ConstructorInfo GetClrConstructorInfo(this IMethodSymbol constructorSymbol, CodeGenerator codeGen)
    {
        if (constructorSymbol is null)
            throw new ArgumentNullException(nameof(constructorSymbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        if (codeGen.TryGetRuntimeConstructor(constructorSymbol, out var cached))
            return cached;

        return constructorSymbol switch
        {
            IMethodSymbol wrapper when wrapper.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, wrapper)
                => codeGen.CacheRuntimeConstructor(constructorSymbol, underlying.GetClrConstructorInfo(codeGen)),
            IAliasSymbol aliasSymbol when aliasSymbol.UnderlyingSymbol is IMethodSymbol underlying
                => codeGen.CacheRuntimeConstructor(constructorSymbol, underlying.GetClrConstructorInfo(codeGen)),
            SourceMethodSymbol sourceConstructor
                => codeGen.CacheRuntimeConstructor(constructorSymbol, (ConstructorInfo)codeGen.GetMemberBuilder(sourceConstructor)),
            SubstitutedMethodSymbol substitutedConstructor
                => codeGen.CacheRuntimeConstructor(constructorSymbol, substitutedConstructor.GetConstructorInfo(codeGen)),
            ConstructedMethodSymbol constructedConstructor
                => codeGen.CacheRuntimeConstructor(constructorSymbol, ResolveConstructedConstructorInfo(constructedConstructor, codeGen)),
            PEMethodSymbol peConstructor => codeGen.CacheRuntimeConstructor(constructorSymbol, ResolveRuntimeConstructorInfo(peConstructor, codeGen)),
            _ => throw new InvalidOperationException($"Unsupported constructor symbol type '{constructorSymbol.GetType()}'.")
        };
    }

    private static MethodInfo ResolveRuntimeMethodInfo(PEMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        var runtimeType = GetContainingRuntimeType(methodSymbol, codeGen);
        var bindingFlags = GetBindingFlags(methodSymbol);

        foreach (var candidate in GetCandidateRuntimeMethods(runtimeType, methodSymbol, bindingFlags))
        {
            if (!RuntimeMethodMatchesSymbol(candidate, methodSymbol, codeGen))
                continue;

            return candidate;
        }

        throw new InvalidOperationException($"Unable to resolve runtime MethodInfo for '{methodSymbol}' on '{runtimeType}'.");
    }

    private static IEnumerable<MethodInfo> GetCandidateRuntimeMethods(Type runtimeType, PEMethodSymbol methodSymbol, BindingFlags bindingFlags)
    {
        var metadataName = methodSymbol.MetadataName;

        var directMatches = runtimeType.GetMember(metadataName, MemberTypes.Method, bindingFlags)
            .OfType<MethodInfo>()
            .ToArray();

        if (directMatches.Length > 0)
            return directMatches;

        return runtimeType.GetMethods(bindingFlags)
            .Where(m => string.Equals(m.Name, metadataName, StringComparison.Ordinal));
    }

    private static ConstructorInfo ResolveRuntimeConstructorInfo(PEMethodSymbol constructorSymbol, CodeGenerator codeGen)
    {
        if (!constructorSymbol.IsConstructor && constructorSymbol.MethodKind != MethodKind.StaticConstructor)
            throw new InvalidOperationException($"Symbol '{constructorSymbol}' is not a constructor.");

        var runtimeType = GetContainingRuntimeType(constructorSymbol, codeGen);
        var bindingFlags = GetBindingFlags(constructorSymbol);

        foreach (var candidate in runtimeType.GetConstructors(bindingFlags))
        {
            if (!RuntimeConstructorMatchesSymbol(candidate, constructorSymbol, codeGen))
                continue;

            return candidate;
        }

        throw new InvalidOperationException($"Unable to resolve runtime ConstructorInfo for '{constructorSymbol}' on '{runtimeType}'.");
    }

    private static Type GetContainingRuntimeType(PEMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        if (methodSymbol.ContainingType is null)
            throw new InvalidOperationException($"Method symbol '{methodSymbol}' is missing a containing type.");

        return methodSymbol.ContainingType.GetClrTypeTreatingUnitAsVoid(codeGen);
    }

    private static BindingFlags GetBindingFlags(IMethodSymbol methodSymbol)
    {
        var flags = BindingFlags.Public | BindingFlags.NonPublic;
        flags |= methodSymbol.IsStatic ? BindingFlags.Static : BindingFlags.Instance;
        flags |= BindingFlags.DeclaredOnly;
        return flags;
    }

    private static bool RuntimeMethodMatchesSymbol(MethodInfo candidate, PEMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        if (!string.Equals(candidate.Name, methodSymbol.MetadataName, StringComparison.Ordinal))
            return false;

        if (!RuntimeTypeMatches(candidate.DeclaringType, methodSymbol.ContainingType, codeGen))
            return false;

        if (candidate.IsGenericMethodDefinition)
        {
            if (!methodSymbol.IsGenericMethod)
                return false;

            if (candidate.GetGenericArguments().Length != methodSymbol.TypeParameters.Length)
                return false;
        }
        else if (methodSymbol.IsGenericMethod)
        {
            return false;
        }

        if (!ParametersMatch(candidate.GetParameters(), methodSymbol.Parameters, codeGen))
            return false;

        return ReturnTypesMatch(candidate.ReturnType, methodSymbol.ReturnType, codeGen);
    }

    private static bool RuntimeConstructorMatchesSymbol(ConstructorInfo candidate, PEMethodSymbol constructorSymbol, CodeGenerator codeGen)
    {
        if (!string.Equals(candidate.Name, constructorSymbol.MetadataName, StringComparison.Ordinal))
            return false;

        if (!RuntimeTypeMatches(candidate.DeclaringType, constructorSymbol.ContainingType, codeGen))
            return false;

        return ParametersMatch(candidate.GetParameters(), constructorSymbol.Parameters, codeGen);
    }

    private static bool RuntimeTypeMatches(Type? runtimeType, ITypeSymbol? symbolType, CodeGenerator codeGen)
    {
        if (runtimeType is null || symbolType is null)
            return false;

        var expected = symbolType.GetClrTypeTreatingUnitAsVoid(codeGen);

        if (runtimeType == expected)
            return true;

        if (runtimeType.IsGenericTypeDefinition && expected.IsGenericType)
            return runtimeType == expected.GetGenericTypeDefinition();

        if (runtimeType.IsGenericType && expected.IsGenericTypeDefinition)
            return runtimeType.GetGenericTypeDefinition() == expected;

        return false;
    }

    internal static bool ParametersMatch(ParameterInfo[] runtimeParameters, ImmutableArray<IParameterSymbol> parameterSymbols, CodeGenerator codeGen)
    {
        if (runtimeParameters.Length != parameterSymbols.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (!ParameterMatches(runtimeParameters[i], parameterSymbols[i], codeGen))
                return false;
        }

        return true;
    }

    private static bool ParameterMatches(ParameterInfo runtimeParameter, IParameterSymbol symbolParameter, CodeGenerator codeGen)
    {
        if (symbolParameter.RefKind == RefKind.Out && !runtimeParameter.IsOut)
            return false;

        if (symbolParameter.RefKind == RefKind.Ref && !runtimeParameter.ParameterType.IsByRef)
            return false;

        if (symbolParameter.RefKind == RefKind.In && !(runtimeParameter.IsIn || runtimeParameter.ParameterType.IsByRef))
            return false;

        return TypesEquivalent(runtimeParameter.ParameterType, symbolParameter.Type, codeGen);
    }

    internal static bool ReturnTypesMatch(Type runtimeReturnType, ITypeSymbol symbolReturnType, CodeGenerator codeGen)
    {
        if (symbolReturnType.SpecialType == SpecialType.System_Void)
            return runtimeReturnType == typeof(void);

        if (symbolReturnType.SpecialType == SpecialType.System_Unit)
        {
            if (runtimeReturnType == typeof(void))
                return true;

            if (codeGen.UnitType is not null)
                return TypesEquivalentCore(runtimeReturnType, codeGen.UnitType);

            return false;
        }

        return TypesEquivalent(runtimeReturnType, symbolReturnType, codeGen);
    }

    internal static bool TypesEquivalent(Type runtimeType, ITypeSymbol symbolType, CodeGenerator codeGen)
    {
        if (symbolType is ITypeParameterSymbol typeParameter)
            return RuntimeTypeMatchesTypeParameter(runtimeType, typeParameter);

        if (symbolType.SpecialType == SpecialType.System_Unit)
        {
            if (runtimeType == typeof(void))
                return true;

            if (codeGen.UnitType is not null)
                return TypesEquivalentCore(runtimeType, codeGen.UnitType);

            return false;
        }

        if (symbolType is INamedTypeSymbol namedTuple && !namedTuple.TupleElements.IsDefaultOrEmpty && namedTuple.TupleElements.Length > 0)
            return RuntimeTupleMatchesSymbol(runtimeType, namedTuple.TupleElements, codeGen);

        var expectedType = symbolType.GetClrType(codeGen);
        return TypesEquivalentCore(runtimeType, expectedType);
    }

    private static bool RuntimeTypeMatchesTypeParameter(Type runtimeType, ITypeParameterSymbol typeParameter)
    {
        if (!runtimeType.IsGenericParameter)
            return false;

        bool isMethodParameter = typeParameter.ContainingSymbol is IMethodSymbol;
        if (isMethodParameter && runtimeType.DeclaringMethod is null)
            return false;

        if (!isMethodParameter && runtimeType.DeclaringMethod is not null)
            return false;

        int? ordinal = typeParameter switch
        {
            PETypeParameterSymbol pe => pe.Ordinal,
            SourceTypeParameterSymbol source => source.Ordinal,
            _ => null
        };

        if (ordinal.HasValue && runtimeType.GenericParameterPosition != ordinal.Value)
            return false;

        return string.Equals(runtimeType.Name, typeParameter.Name, StringComparison.Ordinal);
    }

    private static ConstructorInfo ResolveConstructedConstructorInfo(ConstructedMethodSymbol constructedConstructor, CodeGenerator codeGen)
    {
        if (constructedConstructor is null)
            throw new ArgumentNullException(nameof(constructedConstructor));

        if (constructedConstructor.Definition is SourceMethodSymbol sourceDefinition &&
            codeGen.TryGetMemberBuilder(sourceDefinition, constructedConstructor.TypeArguments, out var cachedMember) &&
            cachedMember is ConstructorInfo cachedConstructor)
        {
            return cachedConstructor;
        }

        var definitionInfo = constructedConstructor.Definition.GetClrConstructorInfo(codeGen);
        var declaringType = definitionInfo.DeclaringType;

        if (declaringType is null)
            return definitionInfo;

        if (constructedConstructor.TypeArguments.IsDefaultOrEmpty || constructedConstructor.TypeArguments.Length == 0)
            return definitionInfo;

        if (!declaringType.IsGenericTypeDefinition && !declaringType.ContainsGenericParameters)
            return definitionInfo;

        var runtimeArguments = constructedConstructor.TypeArguments
            .Select(arg => arg.GetClrType(codeGen))
            .ToArray();

        var constructedRuntimeType = declaringType.IsGenericTypeDefinition
            ? declaringType.MakeGenericType(runtimeArguments)
            : declaringType;

        if (constructedRuntimeType is TypeBuilder typeBuilder)
        {
            var constructed = TypeBuilder.GetConstructor(typeBuilder, definitionInfo)
                ?? throw new InvalidOperationException($"Unable to map constructed constructor '{constructedConstructor}' to runtime info.");

            if (constructedConstructor.Definition is SourceMethodSymbol sourceConstructor)
                codeGen.AddMemberBuilder(sourceConstructor, constructed, constructedConstructor.TypeArguments);

            return constructed;
        }

        var parameterTypes = constructedConstructor.Parameters
            .Select(p => p.Type.GetClrTypeTreatingUnitAsVoid(codeGen))
            .ToArray();

        var resolved = constructedRuntimeType.GetConstructor(
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            types: parameterTypes,
            modifiers: null);

        if (resolved is not null)
        {
            if (constructedConstructor.Definition is SourceMethodSymbol sourceConstructor)
                codeGen.AddMemberBuilder(sourceConstructor, resolved, constructedConstructor.TypeArguments);
            return resolved;
        }

        return definitionInfo;
    }

    private static bool TypesEquivalentCore(Type left, Type right)
    {
        if (left == right)
            return true;

        if (left.IsByRef || right.IsByRef)
        {
            if (left.IsByRef != right.IsByRef)
                return false;

            return TypesEquivalentCore(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsPointer || right.IsPointer)
        {
            if (left.IsPointer != right.IsPointer)
                return false;

            return TypesEquivalentCore(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsArray || right.IsArray)
        {
            if (left.IsArray != right.IsArray)
                return false;

            if (left.GetArrayRank() != right.GetArrayRank())
                return false;

            return TypesEquivalentCore(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsGenericType || right.IsGenericType)
        {
            if (left.IsGenericType != right.IsGenericType)
                return false;

            if (left.IsGenericTypeDefinition != right.IsGenericTypeDefinition)
            {
                if (left.IsGenericTypeDefinition)
                    left = left.MakeGenericType(right.GetGenericArguments());
                else if (right.IsGenericTypeDefinition)
                    right = right.MakeGenericType(left.GetGenericArguments());
            }

            if (left.GetGenericTypeDefinition() != right.GetGenericTypeDefinition())
                return false;

            var leftArgs = left.GetGenericArguments();
            var rightArgs = right.GetGenericArguments();

            if (leftArgs.Length != rightArgs.Length)
                return false;

            for (var i = 0; i < leftArgs.Length; i++)
            {
                if (!TypesEquivalentCore(leftArgs[i], rightArgs[i]))
                    return false;
            }

            return true;
        }

        return left == right;
    }

    private static bool RuntimeTupleMatchesSymbol(Type runtimeType, ImmutableArray<IFieldSymbol> tupleElements, CodeGenerator codeGen)
    {
        var runtimeElementTypes = FlattenValueTupleElementTypes(runtimeType);
        if (runtimeElementTypes is null)
            return false;

        if (runtimeElementTypes.Length != tupleElements.Length)
            return false;

        for (var i = 0; i < runtimeElementTypes.Length; i++)
        {
            if (!TypesEquivalent(runtimeElementTypes[i], tupleElements[i].Type, codeGen))
                return false;
        }

        return true;
    }

    private static Type[]? FlattenValueTupleElementTypes(Type runtimeType)
    {
        if (!IsValueTupleType(runtimeType))
            return null;

        if (!runtimeType.IsGenericType)
            return Array.Empty<Type>();

        var genericArguments = runtimeType.GetGenericArguments();

        if (genericArguments.Length <= 7)
            return genericArguments;

        if (genericArguments.Length != 8)
            return null;

        var nested = FlattenValueTupleElementTypes(genericArguments[7]);
        if (nested is null)
            return null;

        var result = new Type[7 + nested.Length];
        Array.Copy(genericArguments, result, 7);
        Array.Copy(nested, 0, result, 7, nested.Length);
        return result;
    }

    private static bool IsValueTupleType(Type runtimeType)
    {
        var definition = runtimeType;
        if (runtimeType.IsGenericType)
            definition = runtimeType.GetGenericTypeDefinition();

        var fullName = definition.FullName ?? definition.Name;
        return fullName == "System.ValueTuple" || fullName.StartsWith("System.ValueTuple`", StringComparison.Ordinal);
    }
}
