using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using static Raven.CodeAnalysis.CodeGen.DebugUtils;

namespace Raven.CodeAnalysis;

internal static class MethodSymbolCodeGenResolver
{
    internal static MethodInfo GetClrMethodInfo(IMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        if (methodSymbol is null)
            throw new ArgumentNullException(nameof(methodSymbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        var useStableCache = CanUseStableMethodCache(methodSymbol);
        if (useStableCache && codeGen.TryGetRuntimeMethod(methodSymbol, out var cached))
            return cached;

        if (methodSymbol is IAliasSymbol aliasSymbol &&
            aliasSymbol.UnderlyingSymbol is IMethodSymbol underlyingMethod)
        {
            return codeGen.CacheRuntimeMethod(methodSymbol, GetClrMethodInfo(underlyingMethod, codeGen));
        }

        MethodInfo resolved = methodSymbol switch
        {
            SourceMethodSymbol sourceMethod
                => (MethodInfo)codeGen.GetMemberBuilder(sourceMethod),
            SubstitutedMethodSymbol substitutedMethod
                => substitutedMethod.GetMethodInfo(codeGen),
            ConstructedMethodSymbol constructedMethod
                => constructedMethod.GetMethodInfo(codeGen),
            PEMethodSymbol peMethod
                => ResolveRuntimeMethodInfo(peMethod, codeGen),
            _ when methodSymbol.UnderlyingSymbol is IMethodSymbol underlying &&
                   !ReferenceEquals(underlying, methodSymbol)
                => ResolveWrappedMethodInfo(methodSymbol, underlying, codeGen),
            _ => throw new InvalidOperationException($"Unsupported method symbol type '{methodSymbol.GetType()}'.")
        };

        var shouldSkipClosureFix =
            methodSymbol.UnderlyingSymbol is IMethodSymbol &&
            methodSymbol.ContainingType is INamedTypeSymbol wrapperContainingType &&
            ContainsMethodOwnedTypeParameters(wrapperContainingType);

        // IMPORTANT: Reflection over TypeBuilderInstantiation can produce MethodInfo signatures
        // that still reference open generic parameters (!0/!1) in nested out/byref types.
        // That yields invalid IL when emitted (InvalidProgramException). Fix by mapping the
        // method from the generic type definition onto the constructed runtime type.
        if (!shouldSkipClosureFix)
            resolved = EnsureClosedConstructedMethodInfo(methodSymbol, resolved, codeGen);
        if (CodeGenFlags.PrintDebug)
        {
            PrintDebug(
                $"[CodeGen:Method] Resolved {methodSymbol} -> {resolved.DeclaringType?.FullName}::{resolved.Name} " +
                $"(containsGenericParameters={resolved.ContainsGenericParameters})");

            if (resolved.DeclaringType is { IsGenericType: true } declaringType)
            {
                var genericOwnerInfo = string.Join(
                    ", ",
                    declaringType.GetGenericArguments().Select(argument =>
                    {
                        if (!argument.IsGenericParameter)
                            return $"{argument} [owner=n/a]";

                        var owner = argument.DeclaringMethod is null ? "type" : "method";
                        return $"{argument} [owner={owner}, pos={argument.GenericParameterPosition}]";
                    }));

                PrintDebug(
                    $"[CodeGen:Method] DeclaringTypeArgs {resolved.DeclaringType.FullName}::{resolved.Name} => {genericOwnerInfo}");
            }
        }

        if (useStableCache)
            return codeGen.CacheRuntimeMethod(methodSymbol, resolved);

        return resolved;
    }

    private static bool CanUseStableMethodCache(IMethodSymbol methodSymbol)
    {
        if (methodSymbol is ConstructedMethodSymbol or SubstitutedMethodSymbol)
            return false;

        if (methodSymbol.UnderlyingSymbol is IMethodSymbol underlying &&
            !ReferenceEquals(underlying, methodSymbol))
        {
            return false;
        }

        return methodSymbol.ContainingType is not INamedTypeSymbol containingType ||
               !ContainsMethodOwnedTypeParameters(containingType);
    }

    private static MethodInfo ResolveWrappedMethodInfo(
        IMethodSymbol wrapper,
        IMethodSymbol underlying,
        CodeGenerator codeGen)
    {
        var resolvedUnderlying = GetClrMethodInfo(underlying, codeGen);

        var containingType = wrapper.ContainingType;
        if (containingType is null)
            return resolvedUnderlying;

        var containingClrType = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoidForMethodBody(containingType, codeGen);
        if (CodeGenFlags.PrintDebug && containingType.IsGenericType)
        {
            var args = string.Join(
                ", ",
                containingType.TypeArguments.Select(argument =>
                {
                    if (argument is ITypeParameterSymbol typeParameter)
                        return $"{typeParameter.Name}[owner={typeParameter.OwnerKind}]";
                    return argument.ToDisplayString();
                }));
            PrintDebug($"[CodeGen:Method] Wrapper containing type {containingType} args=[{args}]");
        }

        if (ContainsMethodOwnedTypeParameters(containingType))
        {
            try
            {
                var definitionMethod = resolvedUnderlying;
                if (definitionMethod.DeclaringType is not null &&
                    definitionMethod.DeclaringType.IsGenericType &&
                    !definitionMethod.DeclaringType.IsGenericTypeDefinition)
                {
                    var definitionType = definitionMethod.DeclaringType.GetGenericTypeDefinition();
                    definitionMethod = definitionType
                        .GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)
                        .FirstOrDefault(candidate => MethodShapeEquals(candidate, resolvedUnderlying))
                        ?? definitionMethod;
                }

                var projectedDeclaringType = containingClrType;
                if (definitionMethod.DeclaringType is { IsGenericType: true } declaringType)
                {
                    var declaringTypeDefinition = declaringType.IsGenericTypeDefinition
                        ? declaringType
                        : declaringType.GetGenericTypeDefinition();

                    var projectedArguments = containingType.TypeArguments
                        .Select(argument =>
                        {
                            if (argument is ITypeParameterSymbol typeParameter &&
                                typeParameter.OwnerKind == TypeParameterOwnerKind.Method &&
                                typeParameter.Ordinal >= 0)
                            {
                                if (codeGen.TryResolveRuntimeTypeParameter(typeParameter, RuntimeTypeUsage.MethodBody, out var resolvedType))
                                    return resolvedType;

                                return Type.MakeGenericMethodParameter(typeParameter.Ordinal);
                            }

                            return TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoidForMethodBody(argument, codeGen);
                        })
                        .ToArray();

                    if (declaringTypeDefinition.GetGenericArguments().Length == projectedArguments.Length)
                        projectedDeclaringType = declaringTypeDefinition.MakeGenericType(projectedArguments);
                }

                var directMatch = projectedDeclaringType
                    .GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)
                    .FirstOrDefault(candidate => MethodShapeEquals(candidate, definitionMethod));
                if (directMatch is not null)
                    return directMatch;

                var methodProjectedToWrapperType = TypeBuilder.GetMethod(projectedDeclaringType, definitionMethod);
                if (methodProjectedToWrapperType is not null)
                    return methodProjectedToWrapperType;
            }
            catch (NotSupportedException)
            {
            }
            catch (ArgumentException)
            {
            }
            catch (InvalidOperationException)
            {
            }
        }

        var mapped = TryMapToConstructedDeclaringType(containingClrType, resolvedUnderlying)
            ?? TryMapByGenericDefinitionSearch(containingClrType, resolvedUnderlying)
            ?? resolvedUnderlying;

        if (wrapper.IsGenericMethod && wrapper.TypeArguments.Length > 0 && mapped.IsGenericMethodDefinition)
        {
            var methodArguments = wrapper.TypeArguments
                .Select(arg => TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoidForMethodBody(arg, codeGen))
                .ToArray();

            mapped = mapped.MakeGenericMethod(methodArguments);
        }

        return mapped;
    }

    private static bool ContainsMethodOwnedTypeParameters(INamedTypeSymbol type)
        => ContainsMethodOwnedTypeParameters(type, new HashSet<INamedTypeSymbol>(ReferenceEqualityComparer.Instance));

    private static bool ContainsMethodOwnedTypeParameters(
        INamedTypeSymbol type,
        HashSet<INamedTypeSymbol> visiting)
    {
        if (type is null)
            return false;
        if (!visiting.Add(type))
            return false;

        try
        {
            var typeArguments = TypeSubstitution.GetShallowTypeArguments(type);
            if (typeArguments.IsDefaultOrEmpty)
                return false;

            foreach (var argument in typeArguments)
            {
                if (argument is ITypeParameterSymbol typeParameter &&
                    typeParameter.OwnerKind == TypeParameterOwnerKind.Method)
                {
                    return true;
                }

                if (argument is INamedTypeSymbol namedArgument &&
                    ContainsMethodOwnedTypeParameters(namedArgument, visiting))
                {
                    return true;
                }
            }

            return false;
        }
        finally
        {
            visiting.Remove(type);
        }
    }
    // --- Helper for fixing up open generic params in MethodInfo signatures (see GetClrMethodInfo) ---
    private static MethodInfo EnsureClosedConstructedMethodInfo(IMethodSymbol methodSymbol, MethodInfo resolved, CodeGenerator codeGen)
    {
        if (!NeedsGenericClosureFix(resolved))
            return resolved;

        if (CodeGenFlags.PrintDebug)
        {
            PrintDebug(
                $"[CodeGen:Method] Generic closure fix needed for {methodSymbol} from " +
                $"{resolved.DeclaringType?.FullName}::{resolved.Name}");
        }

        // Prefer mapping onto the actual constructed runtime declaring type for the symbol.
        var constructedDeclaringType = methodSymbol.ContainingType is null
            ? null
            : TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(methodSymbol.ContainingType, codeGen);

        var mapped = TryMapToConstructedDeclaringType(constructedDeclaringType, resolved)
            ?? TryMapByGenericDefinitionSearch(constructedDeclaringType, resolved);

        if (mapped is not null && !NeedsGenericClosureFix(mapped))
        {
            if (CodeGenFlags.PrintDebug)
            {
                PrintDebug(
                    $"[CodeGen:Method] Generic closure fix mapped to {mapped.DeclaringType?.FullName}::{mapped.Name} " +
                    $"(containsGenericParameters={mapped.ContainsGenericParameters})");
            }
            return mapped;
        }

        if (CodeGenFlags.PrintDebug)
            PrintDebug($"[CodeGen:Method] Generic closure fix failed for {methodSymbol}, using original MethodInfo");

        return resolved;
    }

    private static bool NeedsGenericClosureFix(MethodInfo mi)
    {
        if (HasMethodScopedGenericLeak(mi))
            return true;

        foreach (var p in mi.GetParameters())
        {
            var pt = p.ParameterType;
            if (HasInvalidGenericScopeInType(pt, allowMethodGeneric: mi.IsGenericMethod))
                return true;

            if (pt.IsByRef)
            {
                var et = pt.GetElementType();
                if (et is not null && HasInvalidGenericScopeInType(et, allowMethodGeneric: mi.IsGenericMethod))
                    return true;
            }
        }

        var rt = mi.ReturnType;
        if (HasInvalidGenericScopeInType(rt, allowMethodGeneric: mi.IsGenericMethod))
            return true;

        if (rt.IsByRef)
        {
            var et = rt.GetElementType();
            if (et is not null && HasInvalidGenericScopeInType(et, allowMethodGeneric: mi.IsGenericMethod))
                return true;
        }

        return false;
    }

    private static bool HasMethodScopedGenericLeak(MethodInfo mi)
    {
        var declaringType = mi.DeclaringType;
        if (declaringType is null)
            return false;

        if (!declaringType.IsGenericType)
            return false;

        foreach (var argument in declaringType.GetGenericArguments())
        {
            if (argument.IsGenericParameter && argument.DeclaringMethod is not null)
                return true;
        }

        return false;
    }

    private static bool HasInvalidGenericScopeInType(Type type, bool allowMethodGeneric)
    {
        if (type.IsByRef || type.IsPointer || type.IsArray)
        {
            var elementType = type.GetElementType();
            return elementType is not null && HasInvalidGenericScopeInType(elementType, allowMethodGeneric);
        }

        if (type.IsGenericParameter)
            return !allowMethodGeneric && type.DeclaringMethod is not null;

        if (!type.IsGenericType)
            return false;

        foreach (var argument in type.GetGenericArguments())
        {
            if (HasInvalidGenericScopeInType(argument, allowMethodGeneric))
                return true;
        }

        return false;
    }

    private static MethodInfo? TryMapToConstructedDeclaringType(Type? constructedDeclaringType, MethodInfo resolved)
    {
        if (constructedDeclaringType is null)
            return null;

        // If already mapped to the constructed declaring type, nothing to do.
        if (resolved.DeclaringType == constructedDeclaringType)
            return resolved;

        // The problematic cases are typically TypeBuilderInstantiation (constructed generic types
        // in a dynamic module). Prefer mapping the method from the generic type definition onto
        // the constructed runtime type.
        static bool IsDynamicAssembly(Type type)
        {
            try
            {
                return type.Assembly.IsDynamic;
            }
            catch (NotSupportedException)
            {
                // Signature placeholder types (e.g., Type.MakeGenericMethodParameter)
                // do not expose Assembly metadata.
                return false;
            }
        }

        if (IsDynamicAssembly(constructedDeclaringType))
        {
            try
            {
                // TypeBuilder.GetMethod expects a MethodInfo from the generic type definition.
                var def = resolved;

                if (def.DeclaringType is not null &&
                    def.DeclaringType.IsGenericType &&
                    !def.DeclaringType.IsGenericTypeDefinition)
                {
                    var defType = def.DeclaringType.GetGenericTypeDefinition();
                    def = defType
                        .GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)
                        .FirstOrDefault(m => MethodShapeEquals(m, resolved))
                        ?? def;
                }

                // IMPORTANT: Use the overload that takes `Type` (works for TypeBuilderInstantiation).
                var mapped = TypeBuilder.GetMethod(constructedDeclaringType, def);
                if (CodeGenFlags.PrintDebug)
                {
                    PrintDebug(
                        $"[CodeGen:Method] TryMapToConstructedDeclaringType(dynamic) {resolved.DeclaringType?.FullName}::{resolved.Name} -> " +
                        $"{mapped?.DeclaringType?.FullName}::{mapped?.Name} (null={mapped is null})");
                }
                return mapped;
            }
            catch
            {
                if (CodeGenFlags.PrintDebug)
                {
                    PrintDebug(
                        $"[CodeGen:Method] TryMapToConstructedDeclaringType(dynamic) threw for {resolved.DeclaringType?.FullName}::{resolved.Name}");
                }
                return null;
            }
        }

        // Runtime generic types that still contain generic parameters (e.g. framework generic types
        // constructed with GenericTypeParameterBuilder arguments) can surface method signatures that
        // incorrectly use method-scoped generic slots. TypeBuilder.GetMethod can still project the
        // definition method onto the constructed runtime type in these cases.
        if (constructedDeclaringType.IsGenericType && constructedDeclaringType.ContainsGenericParameters)
        {
            try
            {
                var def = resolved;
                if (def.DeclaringType is not null &&
                    def.DeclaringType.IsGenericType &&
                    !def.DeclaringType.IsGenericTypeDefinition)
                {
                    var defType = def.DeclaringType.GetGenericTypeDefinition();
                    def = defType
                        .GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)
                        .FirstOrDefault(m => MethodShapeEquals(m, resolved))
                        ?? def;
                }

                return TypeBuilder.GetMethod(constructedDeclaringType, def);
            }
            catch
            {
                return null;
            }
        }

        // For regular runtime constructed types, try to re-resolve directly.
        if (constructedDeclaringType.IsGenericType && !constructedDeclaringType.ContainsGenericParameters)
        {
            try
            {
                var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
                var candidates = constructedDeclaringType.GetMethods(flags).Where(m => MethodShapeEquals(m, resolved));
                var mapped = candidates.FirstOrDefault();
                if (CodeGenFlags.PrintDebug)
                {
                    PrintDebug(
                        $"[CodeGen:Method] TryMapToConstructedDeclaringType(runtime-closed) {resolved.DeclaringType?.FullName}::{resolved.Name} -> " +
                        $"{mapped?.DeclaringType?.FullName}::{mapped?.Name} (null={mapped is null})");
                }
                return mapped;
            }
            catch
            {
                if (CodeGenFlags.PrintDebug)
                {
                    PrintDebug(
                        $"[CodeGen:Method] TryMapToConstructedDeclaringType(runtime-closed) threw for {resolved.DeclaringType?.FullName}::{resolved.Name}");
                }
                return null;
            }
        }

        return null;
    }

    private static MethodInfo? TryMapByGenericDefinitionSearch(Type? constructedDeclaringType, MethodInfo resolved)
    {
        if (constructedDeclaringType is null)
            return null;

        if (!constructedDeclaringType.IsGenericType)
            return null;

        Type defType;
        try
        {
            defType = constructedDeclaringType.IsGenericTypeDefinition
                ? constructedDeclaringType
                : constructedDeclaringType.GetGenericTypeDefinition();
        }
        catch
        {
            return null;
        }

        var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;

        MethodInfo? defMethod = null;
        foreach (var m in defType.GetMethods(flags))
        {
            if (MethodShapeEquals(m, resolved))
            {
                defMethod = m;
                break;
            }
        }

        if (defMethod is null)
            return null;

        // Map definition method onto constructed type.
        static bool IsDynamicAssembly(Type type)
        {
            try
            {
                return type.Assembly.IsDynamic;
            }
            catch (NotSupportedException)
            {
                return false;
            }
        }

        if (IsDynamicAssembly(constructedDeclaringType))
        {
            try { return TypeBuilder.GetMethod(constructedDeclaringType, defMethod); }
            catch { return null; }
        }

        try
        {
            // On normal runtime types, reflection should already return closed signatures, but try anyway.
            return constructedDeclaringType.GetMethods(flags).FirstOrDefault(m => MethodShapeEquals(m, resolved));
        }
        catch
        {
            return null;
        }
    }

    private static bool MethodShapeEquals(MethodInfo candidate, MethodInfo reference)
    {
        if (!string.Equals(candidate.Name, reference.Name, StringComparison.Ordinal))
            return false;

        if (candidate.IsStatic != reference.IsStatic)
            return false;

        // Treat "generic" as shape, regardless of whether we're looking at the definition or a constructed method.
        if (candidate.IsGenericMethod != reference.IsGenericMethod)
            return false;

        if (candidate.IsGenericMethod)
        {
            if (candidate.GetGenericArguments().Length != reference.GetGenericArguments().Length)
                return false;
        }

        var cp = candidate.GetParameters();
        var rp = reference.GetParameters();
        if (cp.Length != rp.Length)
            return false;

        for (var i = 0; i < cp.Length; i++)
        {
            // Compare ByRef-ness and Out-ness (this is what matters for the !0 bug)
            if (cp[i].ParameterType.IsByRef != rp[i].ParameterType.IsByRef)
                return false;

            if (cp[i].IsOut != rp[i].IsOut)
                return false;

            // If both are non-byref, also compare the raw parameter type name as a weak tie-breaker.
            if (!cp[i].ParameterType.IsByRef &&
                !string.Equals(cp[i].ParameterType.Name, rp[i].ParameterType.Name, StringComparison.Ordinal))
            {
                return false;
            }
        }

        return true;
    }

    internal static ConstructorInfo GetClrConstructorInfo(IMethodSymbol constructorSymbol, CodeGenerator codeGen)
    {
        if (constructorSymbol is null)
            throw new ArgumentNullException(nameof(constructorSymbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        constructorSymbol = EnsureConstructedConstructor(constructorSymbol);

        if (codeGen.TryGetRuntimeConstructor(constructorSymbol, out var cached))
            return cached;

        return constructorSymbol switch
        {
            IMethodSymbol wrapper when wrapper.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, wrapper)
                => codeGen.CacheRuntimeConstructor(constructorSymbol, GetClrConstructorInfo(underlying, codeGen)),
            IAliasSymbol aliasSymbol when aliasSymbol.UnderlyingSymbol is IMethodSymbol underlying
                => codeGen.CacheRuntimeConstructor(constructorSymbol, GetClrConstructorInfo(underlying, codeGen)),
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

    private static IMethodSymbol EnsureConstructedConstructor(IMethodSymbol constructorSymbol)
    {
        if (constructorSymbol is null)
            throw new ArgumentNullException(nameof(constructorSymbol));

        if (constructorSymbol is SubstitutedMethodSymbol || constructorSymbol is ConstructedMethodSymbol)
            return constructorSymbol;

        if (constructorSymbol.ContainingType is not ConstructedNamedTypeSymbol constructed)
            return constructorSymbol;

        return new SubstitutedMethodSymbol(constructorSymbol, constructed);
    }

    private static MethodInfo ResolveRuntimeMethodInfo(PEMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        var runtimeType = GetContainingRuntimeType(methodSymbol, codeGen);
        var bindingFlags = GetBindingFlags(methodSymbol);
        var metadataMethod = methodSymbol.GetMethodBase() as MethodInfo;

        foreach (var candidate in GetCandidateRuntimeMethods(runtimeType, methodSymbol, bindingFlags))
        {
            if (!RuntimeMethodMatchesSymbol(candidate, methodSymbol, metadataMethod, codeGen))
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
        var metadataCtor = constructorSymbol.GetMethodBase() as ConstructorInfo;

        foreach (var candidate in runtimeType.GetConstructors(bindingFlags))
        {
            if (!RuntimeConstructorMatchesSymbol(candidate, constructorSymbol, metadataCtor, codeGen))
                continue;

            return candidate;
        }

        throw new InvalidOperationException($"Unable to resolve runtime ConstructorInfo for '{constructorSymbol}' on '{runtimeType}'.");
    }

    private static Type GetContainingRuntimeType(PEMethodSymbol methodSymbol, CodeGenerator codeGen)
    {
        if (methodSymbol.ContainingType is null)
            throw new InvalidOperationException($"Method symbol '{methodSymbol}' is missing a containing type.");

        return TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(methodSymbol.ContainingType, codeGen);
    }

    private static BindingFlags GetBindingFlags(IMethodSymbol methodSymbol)
    {
        var flags = BindingFlags.Public | BindingFlags.NonPublic;
        flags |= methodSymbol.IsStatic ? BindingFlags.Static : BindingFlags.Instance;
        flags |= BindingFlags.DeclaredOnly;
        return flags;
    }

    private static bool RuntimeMethodMatchesSymbol(MethodInfo candidate, PEMethodSymbol methodSymbol, MethodInfo? metadataMethod, CodeGenerator codeGen)
    {
        if (!string.Equals(candidate.Name, methodSymbol.MetadataName, StringComparison.Ordinal))
            return false;

        if (!RuntimeTypeMatches(candidate.DeclaringType, methodSymbol.ContainingType, codeGen))
            return false;

        if (metadataMethod is not null)
            return RuntimeMethodMatchesMetadataSignature(candidate, metadataMethod);

        if (!ParametersMatch(candidate.GetParameters(), methodSymbol.Parameters, codeGen))
            return false;

        return ReturnTypesMatch(candidate.ReturnType, methodSymbol.ReturnType, codeGen);
    }

    private static bool RuntimeConstructorMatchesSymbol(ConstructorInfo candidate, PEMethodSymbol constructorSymbol, ConstructorInfo? metadataCtor, CodeGenerator codeGen)
    {
        if (!string.Equals(candidate.Name, constructorSymbol.MetadataName, StringComparison.Ordinal))
            return false;

        if (!RuntimeTypeMatches(candidate.DeclaringType, constructorSymbol.ContainingType, codeGen))
            return false;

        if (metadataCtor is not null)
            return RuntimeConstructorMatchesMetadataSignature(candidate, metadataCtor);

        return ParametersMatch(candidate.GetParameters(), constructorSymbol.Parameters, codeGen);
    }

    private static bool RuntimeMethodMatchesMetadataSignature(MethodInfo candidate, MethodInfo metadataMethod)
    {
        if (candidate.IsStatic != metadataMethod.IsStatic)
            return false;

        if (candidate.IsGenericMethod != metadataMethod.IsGenericMethod)
            return false;

        if (candidate.IsGenericMethod &&
            candidate.GetGenericArguments().Length != metadataMethod.GetGenericArguments().Length)
            return false;

        if (!ParameterSignaturesMatch(candidate.GetParameters(), metadataMethod.GetParameters()))
            return false;

        return TypeSignatureEquals(candidate.ReturnType, metadataMethod.ReturnType);
    }

    private static bool RuntimeConstructorMatchesMetadataSignature(ConstructorInfo candidate, ConstructorInfo metadataCtor)
    {
        if (candidate.IsStatic != metadataCtor.IsStatic)
            return false;

        return ParameterSignaturesMatch(candidate.GetParameters(), metadataCtor.GetParameters());
    }

    private static bool ParameterSignaturesMatch(ParameterInfo[] runtimeParameters, ParameterInfo[] metadataParameters)
    {
        if (runtimeParameters.Length != metadataParameters.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (runtimeParameters[i].IsOut != metadataParameters[i].IsOut)
                return false;

            if (!TypeSignatureEquals(runtimeParameters[i].ParameterType, metadataParameters[i].ParameterType))
                return false;
        }

        return true;
    }

    private static bool TypeSignatureEquals(Type runtimeType, Type metadataType)
        => string.Equals(GetTypeSignature(runtimeType), GetTypeSignature(metadataType), StringComparison.Ordinal);

    private static string GetTypeSignature(Type type)
    {
        if (type.IsByRef)
            return $"{GetTypeSignature(type.GetElementType()!)}&";

        if (type.IsPointer)
            return $"{GetTypeSignature(type.GetElementType()!)}*";

        if (type.IsArray)
        {
            var rank = type.GetArrayRank();
            var suffix = rank == 1 ? "[]" : $"[{new string(',', rank - 1)}]";
            return $"{GetTypeSignature(type.GetElementType()!)}{suffix}";
        }

        if (type.IsGenericParameter)
        {
            var kind = type.DeclaringMethod is null ? "!" : "!!";
            return $"{kind}{type.GenericParameterPosition}";
        }

        if (type.IsGenericType)
        {
            var definition = type.IsGenericTypeDefinition ? type : type.GetGenericTypeDefinition();
            var args = type.GetGenericArguments();
            return $"{definition.FullName}<{string.Join(",", args.Select(GetTypeSignature))}>";
        }

        return type.FullName ?? type.Name;
    }

    private static bool RuntimeTypeMatches(Type? runtimeType, ITypeSymbol? symbolType, CodeGenerator codeGen)
    {
        if (runtimeType is null || symbolType is null)
            return false;

        var expected = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(symbolType, codeGen);

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

        var runtimeParameterType = runtimeParameter.ParameterType;
        var symbolParameterType = symbolParameter.Type;

        if (symbolParameter.IsByRefParameter())
        {
            if (!runtimeParameterType.IsByRef)
                return false;

            runtimeParameterType = runtimeParameterType.GetElementType() ?? runtimeParameterType;
            symbolParameterType = symbolParameter.GetByRefElementType();
        }

        return TypesEquivalent(runtimeParameterType, symbolParameterType, codeGen);
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

        var expectedType = TypeSymbolExtensionsForCodeGen.GetClrType(symbolType, codeGen);
        return TypesEquivalentCore(runtimeType, expectedType);
    }

    private static bool RuntimeTypeMatchesTypeParameter(Type runtimeType, ITypeParameterSymbol typeParameter)
    {
        if (!runtimeType.IsGenericParameter)
            return false;

        bool isMethodParameter = typeParameter.OwnerKind == TypeParameterOwnerKind.Method;
        if (isMethodParameter && runtimeType.DeclaringMethod is null)
            return false;

        if (!isMethodParameter && runtimeType.DeclaringMethod is not null)
            return false;

        if (runtimeType.GenericParameterPosition != typeParameter.Ordinal)
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

        var definitionInfo = GetClrConstructorInfo(constructedConstructor.Definition, codeGen);
        var declaringType = definitionInfo.DeclaringType;

        if (declaringType is null)
            return definitionInfo;

        if (constructedConstructor.TypeArguments.IsDefaultOrEmpty || constructedConstructor.TypeArguments.Length == 0)
            return definitionInfo;

        if (!declaringType.IsGenericTypeDefinition && !declaringType.ContainsGenericParameters)
            return definitionInfo;

        var runtimeArguments = constructedConstructor.TypeArguments
            .Select(arg => TypeSymbolExtensionsForCodeGen.GetClrType(arg, codeGen))
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
            .Select(p => TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(p.Type, codeGen))
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
