using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

// Resolves generic method constructions for the .NET backend. Semantic symbols
// provide substitutions; all runtime mappings and caches belong to this emission.
internal static class ConstructedMethodCodeGenResolver
{
    internal static MethodInfo GetMethodInfo(ConstructedMethodSymbol construction, CodeGenerator codeGen)
    {
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        var containingType = construction.ContainingType
            ?? throw new InvalidOperationException("Constructed method is missing a containing type.");

        var containingClrType = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(containingType, codeGen);
        var isTypeBuilderInstantiation = string.Equals(
            containingClrType.GetType().FullName,
            "System.Reflection.Emit.TypeBuilderInstantiation",
            StringComparison.Ordinal);
        var methodSearchType = isTypeBuilderInstantiation
            ? containingClrType.GetGenericTypeDefinition()
            : containingClrType;
        var parameterSymbols = construction.Parameters;
        var returnTypeSymbol = construction.ReturnType;
        var debug = ConstructedMethodDebugging.IsEnabled();
        var typeArguments = construction.TypeArguments.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : construction.TypeArguments;
        var runtimeTypeArguments = typeArguments
            .Select(argument => GetProjectedRuntimeType(construction, argument, codeGen, treatUnitAsVoid: false))
            .ToArray();
        runtimeTypeArguments = NormalizeStateMachineRuntimeTypes(construction, runtimeTypeArguments, codeGen);

        if (methodSearchType is TypeBuilder && construction.Definition is SourceMethodSymbol sourceMethod)
        {
            if (codeGen.GetMemberBuilder(sourceMethod) is not MethodBuilder methodBuilder)
                throw new InvalidOperationException($"Method builder for '{sourceMethod.Name}' is unavailable.");

            MethodInfo method = methodBuilder;
            // Calls on generic owners need a MemberRef on the constructed owner,
            // including an open construction using the caller's type parameters.
            if (containingClrType.IsGenericType)
            {
                var owner = containingClrType.IsGenericTypeDefinition
                    ? containingClrType.MakeGenericType(containingClrType.GetGenericArguments())
                    : containingClrType;
                method = TypeBuilder.GetMethod(owner, methodBuilder);
            }
            return method.IsGenericMethodDefinition
                ? method.MakeGenericMethod(runtimeTypeArguments)
                : method;
        }

        if (debug)
        {
            static string FormatType(Type type)
            {
                if (type is null)
                    return "<null>";

                var owner = "n/a";
                if (type.IsGenericParameter)
                {
                    try
                    {
                        owner = type.DeclaringMethod is null ? "type" : "method";
                    }
                    catch (NotSupportedException)
                    {
                        owner = "signature";
                    }
                }
                var position = type.IsGenericParameter ? type.GenericParameterPosition : -1;
                var formatted = $"{type} (gp={type.IsGenericParameter}, owner={owner}, pos={position})";

                if (!type.IsGenericType)
                    return formatted;

                var args = type.GetGenericArguments();
                var formattedArgs = string.Join(", ", args.Select(FormatType));
                return $"{formatted}[{formattedArgs}]";
            }

            for (var i = 0; i < construction.TypeArguments.Length && i < runtimeTypeArguments.Length; i++)
            {
                var runtimeArg = runtimeTypeArguments[i];
                var symbolArg = construction.TypeArguments[i];
                var symbolDescription = symbolArg is INamedTypeSymbol named
                    ? $"{named.ConstructedFrom}<{string.Join(", ", named.TypeArguments.Select(a => $"{a} (kind={a.TypeKind})"))}>"
                    : symbolArg.ToString();
                Console.Error.WriteLine($"[ConstructedMethodSymbol] Type argument {i}: symbol={symbolDescription} runtime={FormatType(runtimeArg)} containsGP={runtimeArg.ContainsGenericParameters}");
            }
        }

        for (var i = 0; i < construction.TypeArguments.Length && i < runtimeTypeArguments.Length; i++)
        {
            if (construction.TypeArguments[i] is not ITypeParameterSymbol { OwnerKind: TypeParameterOwnerKind.Method } methodTypeParameter)
                continue;

            // Prefer the active runtime mapping first. This lets closure/state-machine
            // lowering remap outer method type parameters onto carrier type parameters
            // without this path forcefully rewriting them back to the original method's
            // generic slots (which produces invalid IL like `!!` inside a display class).
            if (codeGen.TryResolveRuntimeTypeParameter(methodTypeParameter, RuntimeTypeUsage.MethodBody, out var resolvedRuntimeType))
            {
                runtimeTypeArguments[i] = resolvedRuntimeType;
                continue;
            }

            if (methodTypeParameter.DeclaringMethodParameterOwner is IMethodSymbol methodSymbol &&
                runtimeTypeArguments[i] is Type runtimeArgument &&
                runtimeArgument.IsGenericParameter &&
                runtimeArgument.DeclaringMethod is null &&
                TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var remapped))
            {
                runtimeTypeArguments[i] = remapped;
            }
        }

        if (TryResolveFromCachedDefinition(
                construction, codeGen,
                containingClrType,
                isTypeBuilderInstantiation,
                parameterSymbols,
                returnTypeSymbol,
                runtimeTypeArguments,
                out var cached,
                debug))
        {
            return cached;
        }

        const BindingFlags Flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        var peDefinitionMethod = construction.Definition as PEMethodSymbol;
        var usePeDefinitionSymbolMatch = peDefinitionMethod is not null;

        foreach (var method in methodSearchType.GetMethods(Flags))
        {
            MethodInfo candidate = method;

            if (isTypeBuilderInstantiation)
            {
                try
                {
                    var instantiated = TypeBuilder.GetMethod(containingClrType, method);
                    if (instantiated is null)
                        continue;

                    candidate = instantiated;
                }
                catch (ArgumentException)
                {
                    continue;
                }
            }

            if (!string.Equals(candidate.Name, construction.Definition.Name, StringComparison.Ordinal))
                continue;

            if (candidate.IsGenericMethodDefinition != construction.Definition.IsGenericMethod)
            {
                if (!candidate.IsGenericMethodDefinition)
                    continue;
            }

            if (candidate.IsGenericMethodDefinition)
            {
                if (candidate.GetGenericArguments().Length != runtimeTypeArguments.Length)
                    continue;
                candidate = candidate.MakeGenericMethod(runtimeTypeArguments);
            }
            else if (candidate.ContainsGenericParameters)
            {
                continue;
            }

            var candidateParameters = candidate.GetParameters();
            var methodRuntimeArguments = candidate.IsGenericMethod
                ? candidate.GetGenericArguments()
                : Array.Empty<Type>();
            var typeRuntimeArguments = candidate.DeclaringType is not null && candidate.DeclaringType.IsGenericType
                ? candidate.DeclaringType.GetGenericArguments()
                : Array.Empty<Type>();

            if (usePeDefinitionSymbolMatch)
            {
                var candidateDefinition = candidate.IsGenericMethod && !candidate.IsGenericMethodDefinition
                    ? candidate.GetGenericMethodDefinition()
                    : candidate;

                if (!MethodDefinitionMatchesPeSymbol(candidateDefinition, peDefinitionMethod!))
                {
                    if (debug)
                    {
                        Console.Error.WriteLine($"  Rejected candidate {candidate} due to symbol-signature mismatch.");
                    }
                    continue;
                }
            }

            var parametersMatch = ParametersMatch(construction, candidateParameters, parameterSymbols, methodRuntimeArguments, typeRuntimeArguments, codeGen, debug);
            if (!parametersMatch)
            {
                if (debug)
                {
                    Console.Error.WriteLine($"  Rejected candidate {candidate} due to parameter mismatch.");
                    Console.Error.WriteLine($"    Candidate params: {string.Join(", ", candidateParameters.Select(p => p.ParameterType))}");
                }
                continue;
            }

            var normalizedReturnType = SubstituteRuntimeType(candidate.ReturnType, methodRuntimeArguments, typeRuntimeArguments);
            if (!MethodSymbolCodeGenResolver.ReturnTypesMatch(normalizedReturnType, returnTypeSymbol, codeGen))
            {
                if (debug)
                {
                    Console.Error.WriteLine($"  Rejected candidate {candidate} due to return type mismatch: {normalizedReturnType} vs {returnTypeSymbol}.");
                }
                continue;
            }

            return candidate;
        }

        if (debug)
        {
            Console.Error.WriteLine($"[ConstructedMethodSymbol] Unable to resolve '{construction.Definition}' on '{containingClrType}'.");
            Console.Error.WriteLine($"  Parameters: {string.Join(", ", parameterSymbols.Select(p => p.Type.ToString()))}");
            Console.Error.WriteLine($"  Type arguments: {string.Join(", ", construction.TypeArguments.Select(a => a.ToString()))}");
            Console.Error.WriteLine($"  Runtime type arguments: {string.Join(", ", runtimeTypeArguments.Select(t => t?.FullName ?? t?.ToString() ?? "<null>"))}");
        }

        if (runtimeTypeArguments.Length > 0)
        {
            const BindingFlags relaxedFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

            foreach (var method in methodSearchType.GetMethods(relaxedFlags))
            {
                if (!string.Equals(method.Name, construction.Definition.Name, StringComparison.Ordinal))
                    continue;

                if (!method.IsGenericMethodDefinition)
                    continue;

                if (method.GetGenericArguments().Length != runtimeTypeArguments.Length)
                    continue;

                if (method.GetParameters().Length != parameterSymbols.Length)
                    continue;

                try
                {
                    var relaxed = method.MakeGenericMethod(runtimeTypeArguments);
                    if (debug)
                    {
                        Console.Error.WriteLine($"[ConstructedMethodSymbol] Relaxed generic match for '{construction.Definition}' -> {relaxed}");
                    }
                    return relaxed;
                }
                catch (ArgumentException)
                {
                    continue;
                }
            }
        }

        throw new InvalidOperationException($"Unable to resolve constructed method '{construction.Definition.Name}'.");
    }

    private static bool MethodDefinitionMatchesPeSymbol(MethodInfo runtimeDefinition, PEMethodSymbol symbolDefinition)
    {
        if (symbolDefinition.ReflectionMethodBase is MethodInfo metadataMethod)
            return RuntimeDefinitionMatchesMetadataMethod(runtimeDefinition, metadataMethod);

        if (!string.Equals(runtimeDefinition.Name, symbolDefinition.MetadataName, StringComparison.Ordinal))
            return false;

        if (runtimeDefinition.IsStatic != symbolDefinition.IsStatic)
            return false;

        if (runtimeDefinition.IsGenericMethod != symbolDefinition.IsGenericMethod)
            return false;

        if (runtimeDefinition.IsGenericMethod &&
            runtimeDefinition.GetGenericArguments().Length != symbolDefinition.TypeParameters.Length)
            return false;

        var runtimeParameters = runtimeDefinition.GetParameters();
        if (runtimeParameters.Length != symbolDefinition.Parameters.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            var runtimeParameter = runtimeParameters[i];
            var symbolParameter = symbolDefinition.Parameters[i];

            if (symbolParameter.RefKind == RefKind.Out && !runtimeParameter.IsOut)
                return false;

            if (symbolParameter.RefKind == RefKind.Ref && !runtimeParameter.ParameterType.IsByRef)
                return false;

            if (!TypeSignatureEquals(runtimeParameter.ParameterType, symbolParameter.Type))
                return false;
        }

        return TypeSignatureEquals(runtimeDefinition.ReturnType, symbolDefinition.ReturnType);
    }

    private static bool RuntimeDefinitionMatchesMetadataMethod(MethodInfo runtimeDefinition, MethodInfo metadataMethod)
    {
        if (!string.Equals(runtimeDefinition.Name, metadataMethod.Name, StringComparison.Ordinal))
            return false;

        if (runtimeDefinition.IsStatic != metadataMethod.IsStatic)
            return false;

        if (runtimeDefinition.IsGenericMethod != metadataMethod.IsGenericMethod)
            return false;

        if (runtimeDefinition.IsGenericMethod &&
            runtimeDefinition.GetGenericArguments().Length != metadataMethod.GetGenericArguments().Length)
        {
            return false;
        }

        var runtimeParameters = runtimeDefinition.GetParameters();
        var metadataParameters = metadataMethod.GetParameters();
        if (runtimeParameters.Length != metadataParameters.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (!RuntimeTypeMatchesMetadataType(runtimeParameters[i].ParameterType, metadataParameters[i].ParameterType))
                return false;
        }

        return RuntimeTypeMatchesMetadataType(runtimeDefinition.ReturnType, metadataMethod.ReturnType);
    }

    private static bool RuntimeTypeMatchesMetadataType(Type runtimeType, Type metadataType)
    {
        if (runtimeType.IsByRef || metadataType.IsByRef)
        {
            if (runtimeType.IsByRef != metadataType.IsByRef)
                return false;

            return RuntimeTypeMatchesMetadataType(runtimeType.GetElementType()!, metadataType.GetElementType()!);
        }

        if (runtimeType.IsPointer || metadataType.IsPointer)
        {
            if (runtimeType.IsPointer != metadataType.IsPointer)
                return false;

            return RuntimeTypeMatchesMetadataType(runtimeType.GetElementType()!, metadataType.GetElementType()!);
        }

        if (runtimeType.IsArray || metadataType.IsArray)
        {
            if (runtimeType.IsArray != metadataType.IsArray)
                return false;

            if (runtimeType.GetArrayRank() != metadataType.GetArrayRank())
                return false;

            return RuntimeTypeMatchesMetadataType(runtimeType.GetElementType()!, metadataType.GetElementType()!);
        }

        if (runtimeType.IsGenericParameter || metadataType.IsGenericParameter)
        {
            if (runtimeType.IsGenericParameter != metadataType.IsGenericParameter)
                return false;

            if (runtimeType.GenericParameterPosition != metadataType.GenericParameterPosition)
                return false;

            return (runtimeType.DeclaringMethod is not null) == (metadataType.DeclaringMethod is not null);
        }

        if (runtimeType.IsGenericType || metadataType.IsGenericType)
        {
            if (runtimeType.IsGenericType != metadataType.IsGenericType)
                return false;

            var runtimeDefinition = runtimeType.IsGenericTypeDefinition
                ? runtimeType
                : runtimeType.GetGenericTypeDefinition();
            var metadataDefinition = metadataType.IsGenericTypeDefinition
                ? metadataType
                : metadataType.GetGenericTypeDefinition();

            if (!string.Equals(runtimeDefinition.FullName, metadataDefinition.FullName, StringComparison.Ordinal) &&
                !(string.Equals(runtimeDefinition.Name, metadataDefinition.Name, StringComparison.Ordinal) &&
                  string.Equals(runtimeDefinition.Namespace, metadataDefinition.Namespace, StringComparison.Ordinal)))
            {
                return false;
            }

            var runtimeArgs = runtimeType.GetGenericArguments();
            var metadataArgs = metadataType.GetGenericArguments();
            if (runtimeArgs.Length != metadataArgs.Length)
                return false;

            for (var i = 0; i < runtimeArgs.Length; i++)
            {
                if (!RuntimeTypeMatchesMetadataType(runtimeArgs[i], metadataArgs[i]))
                    return false;
            }

            return true;
        }

        if (runtimeType == metadataType)
            return true;

        if (string.Equals(runtimeType.FullName, metadataType.FullName, StringComparison.Ordinal))
            return true;

        return string.Equals(runtimeType.Name, metadataType.Name, StringComparison.Ordinal)
               && string.Equals(runtimeType.Namespace, metadataType.Namespace, StringComparison.Ordinal);
    }

    private static bool TypeSignatureEquals(Type runtimeType, Type metadataType)
        => string.Equals(GetTypeSignature(runtimeType), GetTypeSignature(metadataType), StringComparison.Ordinal);

    private static bool TypeSignatureEquals(Type runtimeType, ITypeSymbol symbolType)
        => string.Equals(GetTypeSignature(runtimeType), GetTypeSignature(symbolType), StringComparison.Ordinal);

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

    private static string GetTypeSignature(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var rank = arrayType.Rank;
            var suffix = rank == 1 ? "[]" : $"[{new string(',', rank - 1)}]";
            return $"{GetTypeSignature(arrayType.ElementType)}{suffix}";
        }

        if (typeSymbol is IPointerTypeSymbol pointerType)
            return $"{GetTypeSignature(pointerType.PointedAtType)}*";

        if (typeSymbol is RefTypeSymbol refType)
            return $"{GetTypeSignature(refType.ElementType)}&";

        if (typeSymbol is ITypeParameterSymbol typeParameter)
        {
            var kind = typeParameter.OwnerKind == TypeParameterOwnerKind.Method ? "!!" : "!";
            return $"{kind}{typeParameter.Ordinal}";
        }

        if (typeSymbol is INamedTypeSymbol namedType)
        {
            var definition = namedType.OriginalDefinition ?? namedType;
            var typeArguments = namedType.TypeArguments;
            var metadataName = definition.ToFullyQualifiedMetadataName();

            if (typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0)
                return metadataName;

            return $"{metadataName}<{string.Join(",", typeArguments.Select(GetTypeSignature))}>";
        }

        return typeSymbol.ToDisplayString();
    }

    private static bool TryResolveFromCachedDefinition(
        ConstructedMethodSymbol construction,
        CodeGenerator codeGen,
        Type containingClrType,
        bool isTypeBuilderInstantiation,
        ImmutableArray<IParameterSymbol> parameterSymbols,
        ITypeSymbol returnTypeSymbol,
        Type[] runtimeTypeArguments,
        out MethodInfo methodInfo,
        bool debug)
    {
        methodInfo = null!;

        if (!TryGetSourceDefinitionSymbol(construction.Definition, out var sourceDefinition))
            return false;

        if (!codeGen.TryGetMemberBuilder(sourceDefinition, construction.TypeArguments, out var member) ||
            member is not MethodInfo definitionMethod)
        {
            if (!codeGen.TryGetMemberBuilder(sourceDefinition, out member) ||
                member is not MethodInfo definitionMethodFromDefinition)
            {
                return false;
            }

            definitionMethod = definitionMethodFromDefinition;
        }

        var candidateDefinition = definitionMethod;

        if (candidateDefinition.IsGenericMethod && !candidateDefinition.IsGenericMethodDefinition)
            candidateDefinition = candidateDefinition.GetGenericMethodDefinition();

        if (isTypeBuilderInstantiation)
        {
            try
            {
                var projected = TypeBuilder.GetMethod(containingClrType, candidateDefinition);
                if (projected is null)
                    return false;
                candidateDefinition = projected;
            }
            catch (ArgumentException)
            {
                return false;
            }
        }

        var candidate = candidateDefinition;

        if (candidate.IsGenericMethodDefinition)
        {
            if (runtimeTypeArguments.Length != candidate.GetGenericArguments().Length)
                return false;

            candidate = candidate.MakeGenericMethod(runtimeTypeArguments);
        }

        if (candidate is MethodBuilder ||
            string.Equals(candidate.GetType().FullName, "System.Reflection.Emit.MethodBuilderInstantiation", StringComparison.Ordinal))
        {
            codeGen.AddMemberBuilder(sourceDefinition, candidate, construction.TypeArguments);
            methodInfo = candidate;
            return true;
        }

        var candidateParameters = candidate.GetParameters();
        var methodRuntimeArguments = candidate.IsGenericMethod
            ? candidate.GetGenericArguments()
            : Array.Empty<Type>();
        var typeRuntimeArguments = candidate.DeclaringType is not null && candidate.DeclaringType.IsGenericType
            ? candidate.DeclaringType.GetGenericArguments()
            : Array.Empty<Type>();

        if (!ParametersMatch(construction, candidateParameters, parameterSymbols, methodRuntimeArguments, typeRuntimeArguments, codeGen, debug))
            return false;

        var normalizedReturnType = SubstituteRuntimeType(candidate.ReturnType, methodRuntimeArguments, typeRuntimeArguments);
        if (!MethodSymbolCodeGenResolver.ReturnTypesMatch(normalizedReturnType, returnTypeSymbol, codeGen))
            return false;

        methodInfo = candidate;
        codeGen.AddMemberBuilder(sourceDefinition, candidate, construction.TypeArguments);
        return true;
    }

    private static bool TryGetSourceDefinitionSymbol(IMethodSymbol methodSymbol, out SourceSymbol sourceSymbol)
    {
        switch (methodSymbol)
        {
            case SourceSymbol source:
                sourceSymbol = source;
                return true;
            case IAliasSymbol alias when alias.UnderlyingSymbol is IMethodSymbol underlyingMethod:
                return TryGetSourceDefinitionSymbol(underlyingMethod, out sourceSymbol);
            default:
                {
                    var originalDefinition = methodSymbol.OriginalDefinition;
                    if (originalDefinition is not null && !ReferenceEquals(originalDefinition, methodSymbol) &&
                        TryGetSourceDefinitionSymbol(originalDefinition, out sourceSymbol))
                    {
                        return true;
                    }

                    var constructedFrom = methodSymbol.ConstructedFrom;
                    if (constructedFrom is not null && !ReferenceEquals(constructedFrom, methodSymbol) &&
                        TryGetSourceDefinitionSymbol(constructedFrom, out sourceSymbol))
                    {
                        return true;
                    }

                    sourceSymbol = null!;
                    return false;
                }
        }
    }

    private static bool TryGetMethodGenericParameter(
        IMethodSymbol methodSymbol,
        int ordinal,
        CodeGenerator codeGen,
        out Type parameter)
    {
        parameter = null!;

        if (TryGetSourceMethod(methodSymbol, out var sourceMethod) &&
            codeGen.TryGetMemberBuilder(sourceMethod, out var member) &&
            member is MethodInfo methodInfo)
        {
            var definition = methodInfo.IsGenericMethodDefinition
                ? methodInfo
                : methodInfo.GetGenericMethodDefinition();

            var arguments = definition.GetGenericArguments();
            if ((uint)ordinal < (uint)arguments.Length)
            {
                parameter = arguments[ordinal];
                return true;
            }
        }

        return false;
    }

    private static Type[] NormalizeStateMachineRuntimeTypes(ConstructedMethodSymbol construction, Type[] runtimeTypeArguments, CodeGenerator codeGen)
    {
        if (runtimeTypeArguments.Length == 0)
            return runtimeTypeArguments;

        var normalized = new Type[runtimeTypeArguments.Length];
        var updated = false;

        for (var i = 0; i < runtimeTypeArguments.Length; i++)
        {
            var symbolArgument = construction.TypeArguments.Length > i ? construction.TypeArguments[i] : null;
            var argument = runtimeTypeArguments[i];
            var normalizedArgument = NormalizeStateMachineRuntimeType(construction, symbolArgument, argument, codeGen);
            normalized[i] = normalizedArgument;

            if (!ReferenceEquals(argument, normalizedArgument))
                updated = true;
        }

        return updated ? normalized : runtimeTypeArguments;
    }

    private static Type NormalizeStateMachineRuntimeType(ConstructedMethodSymbol construction, ITypeSymbol? symbolArgument, Type runtimeArgument, CodeGenerator codeGen)
    {
        if (symbolArgument is INamedTypeSymbol namedArgument &&
            namedArgument.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
            runtimeArgument.IsGenericType)
        {
            var asyncRuntimeParameters = new Type[stateMachine.TypeParameters.Length];

            foreach (var mapping in stateMachine.TypeParameterMappings)
            {
                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, mapping.AsyncParameter.Ordinal, codeGen, out var asyncRuntime))
                {
                    asyncRuntimeParameters[mapping.StateMachineParameter.Ordinal] = asyncRuntime;
                    continue;
                }

                if (codeGen.TryGetRuntimeTypeForTypeParameter(mapping.AsyncParameter, out var asyncResolved))
                    asyncRuntimeParameters[mapping.StateMachineParameter.Ordinal] = asyncResolved;
            }

            runtimeArgument = SubstituteStateMachineRuntimeGenerics(runtimeArgument, asyncRuntimeParameters);
        }

        return SubstituteRuntimeTypeUsingSymbol(construction, runtimeArgument, symbolArgument, codeGen);
    }

    private static Type SubstituteRuntimeTypeUsingSymbol(ConstructedMethodSymbol construction, Type runtimeType, ITypeSymbol? symbolArgument, CodeGenerator codeGen)
    {
        if (symbolArgument is null)
            return runtimeType;

        if (runtimeType.IsByRef)
        {
            var symbolElement = (symbolArgument as RefTypeSymbol)?.ElementType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(construction, runtimeType.GetElementType()!, symbolElement, codeGen);
            return substitutedElement.MakeByRefType();
        }

        if (runtimeType.IsPointer)
        {
            var symbolElement = (symbolArgument as IPointerTypeSymbol)?.PointedAtType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(construction, runtimeType.GetElementType()!, symbolElement, codeGen);
            return substitutedElement.MakePointerType();
        }

        if (runtimeType.IsArray)
        {
            var symbolElement = (symbolArgument as IArrayTypeSymbol)?.ElementType ?? symbolArgument;
            var substitutedElement = SubstituteRuntimeTypeUsingSymbol(construction, runtimeType.GetElementType()!, symbolElement, codeGen);
            return runtimeType.GetArrayRank() == 1
                ? substitutedElement.MakeArrayType()
                : substitutedElement.MakeArrayType(runtimeType.GetArrayRank());
        }

        if (runtimeType.IsGenericParameter)
        {
            return GetProjectedRuntimeType(construction, symbolArgument, codeGen, treatUnitAsVoid: false, isTopLevel: false);
        }

        if (runtimeType.IsGenericType && symbolArgument is INamedTypeSymbol named && named.IsGenericType)
        {
            var definition = runtimeType.IsGenericTypeDefinition
                ? runtimeType
                : runtimeType.GetGenericTypeDefinition();

            var runtimeArguments = runtimeType.GetGenericArguments();
            var symbolArguments = TypeSymbolExtensionsForCodeGen.GetRuntimeTypeArguments(named, definition);
            var substitutedArguments = new Type[runtimeArguments.Length];
            var changed = false;

            for (var i = 0; i < runtimeArguments.Length; i++)
            {
                var symbolArg = i < symbolArguments.Length ? symbolArguments[i] : null;
                substitutedArguments[i] = SubstituteRuntimeTypeUsingSymbol(construction, runtimeArguments[i], symbolArg, codeGen);

                if (!ReferenceEquals(substitutedArguments[i], runtimeArguments[i]))
                    changed = true;
            }

            if (!changed)
                return runtimeType;

            return definition.MakeGenericType(substitutedArguments);
        }

        return runtimeType;
    }

    private static Type SubstituteStateMachineRuntimeGenerics(Type runtimeType, Type[] asyncRuntimeParameters)
    {
        if (runtimeType.IsGenericParameter)
        {
            var position = runtimeType.GenericParameterPosition;
            if ((uint)position < (uint)asyncRuntimeParameters.Length && asyncRuntimeParameters[position] is Type mapped)
                return mapped;

            return runtimeType;
        }

        if (!runtimeType.IsGenericType)
            return runtimeType;

        var definition = runtimeType.IsGenericTypeDefinition ? runtimeType : runtimeType.GetGenericTypeDefinition();
        var arguments = runtimeType.GetGenericArguments();
        var replaced = false;

        for (var i = 0; i < arguments.Length; i++)
        {
            var substituted = SubstituteStateMachineRuntimeGenerics(arguments[i], asyncRuntimeParameters);
            if (!ReferenceEquals(substituted, arguments[i]))
            {
                arguments[i] = substituted;
                replaced = true;
            }
        }

        if (!replaced)
            return runtimeType;

        return definition.MakeGenericType(arguments);
    }

    private static bool TryGetSourceMethod(IMethodSymbol methodSymbol, out SourceMethodSymbol sourceMethod)
    {
        switch (methodSymbol)
        {
            case SourceMethodSymbol source:
                sourceMethod = source;
                return true;
            case IAliasSymbol alias when alias.UnderlyingSymbol is IMethodSymbol aliasMethod &&
                TryGetSourceMethod(aliasMethod, out sourceMethod):
                return true;
        }

        if (methodSymbol.UnderlyingSymbol is IMethodSymbol underlying &&
            !ReferenceEquals(underlying, methodSymbol) &&
            TryGetSourceMethod(underlying, out sourceMethod))
        {
            return true;
        }

        var originalDefinition = methodSymbol.OriginalDefinition;
        if (originalDefinition is not null &&
            !ReferenceEquals(originalDefinition, methodSymbol) &&
            TryGetSourceMethod(originalDefinition, out sourceMethod))
        {
            return true;
        }

        var constructedFrom = methodSymbol.ConstructedFrom;
        if (constructedFrom is not null &&
            !ReferenceEquals(constructedFrom, methodSymbol) &&
            TryGetSourceMethod(constructedFrom, out sourceMethod))
        {
            return true;
        }

        sourceMethod = null!;
        return false;
    }

    private static bool ParametersMatch(ConstructedMethodSymbol construction,
        ParameterInfo[] runtimeParameters,
        ImmutableArray<IParameterSymbol> parameterSymbols,
        Type[] methodRuntimeArguments,
        Type[]? typeRuntimeArguments,
        CodeGenerator codeGen,
        bool debug)
    {
        if (runtimeParameters.Length != parameterSymbols.Length)
            return false;

        for (var i = 0; i < runtimeParameters.Length; i++)
        {
            if (!ParameterMatches(construction, runtimeParameters[i], parameterSymbols[i], methodRuntimeArguments, typeRuntimeArguments, codeGen))
            {
                if (debug)
                {
                    var normalized = SubstituteRuntimeType(runtimeParameters[i].ParameterType, methodRuntimeArguments, typeRuntimeArguments);
                    var symbolType = parameterSymbols[i].IsByRefParameter
                        ? parameterSymbols[i].GetByRefElementType()
                        : parameterSymbols[i].Type;
                    Type expected;
                    try
                    {
                        expected = GetProjectedRuntimeType(construction, symbolType, codeGen, treatUnitAsVoid: true, isTopLevel: false);
                    }
                    catch
                    {
                        expected = TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(symbolType, codeGen);
                    }
                    Console.Error.WriteLine($"    Parameter mismatch at {i}: runtime={normalized} expected={expected} symbol={parameterSymbols[i].Type}");
                }

                return false;
            }
        }

        return true;
    }

    private static bool ParameterMatches(ConstructedMethodSymbol construction,
        ParameterInfo runtimeParameter,
        IParameterSymbol symbolParameter,
        Type[] methodRuntimeArguments,
        Type[]? typeRuntimeArguments,
        CodeGenerator codeGen)
    {
        if (symbolParameter.RefKind == RefKind.Out && !runtimeParameter.IsOut)
            return false;

        if (symbolParameter.RefKind == RefKind.Ref && !runtimeParameter.ParameterType.IsByRef)
            return false;

        if (symbolParameter.RefKind == RefKind.In && !(runtimeParameter.IsIn || runtimeParameter.ParameterType.IsByRef))
            return false;

        var runtimeParameterType = runtimeParameter.ParameterType;
        var symbolParameterType = symbolParameter.Type;

        if (symbolParameter.IsByRefParameter)
        {
            if (!runtimeParameterType.IsByRef)
                return false;

            runtimeParameterType = runtimeParameterType.GetElementType() ?? runtimeParameterType;
            symbolParameterType = symbolParameter.GetByRefElementType();
        }

        var normalizedRuntimeType = SubstituteRuntimeType(runtimeParameterType, methodRuntimeArguments, typeRuntimeArguments);
        var equivalent = MethodSymbolCodeGenResolver.TypesEquivalent(normalizedRuntimeType, symbolParameterType, codeGen);

        if (!equivalent)
        {
            try
            {
                var projectedSymbolType = GetProjectedRuntimeType(construction, symbolParameterType, codeGen, treatUnitAsVoid: true, isTopLevel: false);
                var normalizedKey = GetRuntimeTypeKey(normalizedRuntimeType);
                var projectedKey = GetRuntimeTypeKey(projectedSymbolType);
                if (TypeSignatureEquals(normalizedRuntimeType, projectedSymbolType) ||
                    string.Equals(normalizedRuntimeType.ToString(), projectedSymbolType.ToString(), StringComparison.Ordinal) ||
                    string.Equals(normalizedKey, projectedKey, StringComparison.Ordinal) ||
                    RuntimeTypesEquivalentLoosely(normalizedRuntimeType, projectedSymbolType))
                    equivalent = true;
            }
            catch
            {
                // Keep existing fallback checks below.
            }

            if (equivalent)
                return true;

            var symbolClrType = TypeSymbolExtensionsForCodeGen.GetClrType(symbolParameterType, codeGen);
            if (string.Equals(normalizedRuntimeType.ToString(), symbolClrType.ToString(), StringComparison.Ordinal))
                equivalent = true;
        }

        return equivalent;
    }

    private static string GetRuntimeTypeKey(Type type)
    {
        if (type.IsByRef)
            return $"{GetRuntimeTypeKey(type.GetElementType()!)}&";

        if (type.IsPointer)
            return $"{GetRuntimeTypeKey(type.GetElementType()!)}*";

        if (type.IsArray)
        {
            var rank = type.GetArrayRank();
            var suffix = rank == 1 ? "[]" : $"[{new string(',', rank - 1)}]";
            return $"{GetRuntimeTypeKey(type.GetElementType()!)}{suffix}";
        }

        if (type.IsGenericParameter)
        {
            string ownerPrefix;
            try
            {
                ownerPrefix = type.DeclaringMethod is null ? "!" : "!!";
            }
            catch (NotSupportedException)
            {
                ownerPrefix = type.IsGenericTypeParameter ? "!" : "!!";
            }

            return $"{ownerPrefix}{type.GenericParameterPosition}";
        }

        if (type.IsGenericType)
        {
            var definition = type.IsGenericTypeDefinition ? type : type.GetGenericTypeDefinition();
            var args = type.GetGenericArguments();
            return $"{definition.FullName ?? definition.Name}<{string.Join(",", args.Select(GetRuntimeTypeKey))}>";
        }

        return type.FullName ?? type.Name;
    }

    private static bool RuntimeTypesEquivalentLoosely(Type left, Type right)
    {
        if (ReferenceEquals(left, right) || left == right)
            return true;

        if (left.IsByRef || right.IsByRef)
        {
            if (left.IsByRef != right.IsByRef)
                return false;

            return RuntimeTypesEquivalentLoosely(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsPointer || right.IsPointer)
        {
            if (left.IsPointer != right.IsPointer)
                return false;

            return RuntimeTypesEquivalentLoosely(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsArray || right.IsArray)
        {
            if (left.IsArray != right.IsArray || left.GetArrayRank() != right.GetArrayRank())
                return false;

            return RuntimeTypesEquivalentLoosely(left.GetElementType()!, right.GetElementType()!);
        }

        if (left.IsGenericParameter || right.IsGenericParameter)
        {
            if (left.IsGenericParameter != right.IsGenericParameter)
                return false;

            if (left.GenericParameterPosition != right.GenericParameterPosition)
                return false;

            if (left.IsGenericMethodParameter != right.IsGenericMethodParameter)
                return false;

            if (left.IsGenericTypeParameter != right.IsGenericTypeParameter)
                return false;

            return true;
        }

        if (left.IsGenericType || right.IsGenericType)
        {
            if (left.IsGenericType != right.IsGenericType)
                return false;

            var leftDef = left.IsGenericTypeDefinition ? left : left.GetGenericTypeDefinition();
            var rightDef = right.IsGenericTypeDefinition ? right : right.GetGenericTypeDefinition();
            if (leftDef != rightDef && !string.Equals(leftDef.FullName, rightDef.FullName, StringComparison.Ordinal))
                return false;

            var leftArgs = left.GetGenericArguments();
            var rightArgs = right.GetGenericArguments();
            if (leftArgs.Length != rightArgs.Length)
                return false;

            for (var i = 0; i < leftArgs.Length; i++)
            {
                if (!RuntimeTypesEquivalentLoosely(leftArgs[i], rightArgs[i]))
                    return false;
            }

            return true;
        }

        return string.Equals(left.FullName, right.FullName, StringComparison.Ordinal) ||
               string.Equals(left.Name, right.Name, StringComparison.Ordinal);
    }

    private static Type SubstituteRuntimeType(Type runtimeType, Type[] methodRuntimeArguments, Type[]? typeRuntimeArguments)
    {
        if (runtimeType.IsByRef)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return element.MakeByRefType();
        }

        if (runtimeType.IsPointer)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return element.MakePointerType();
        }

        if (runtimeType.IsArray)
        {
            var element = SubstituteRuntimeType(runtimeType.GetElementType()!, methodRuntimeArguments, typeRuntimeArguments);
            return runtimeType.GetArrayRank() == 1
                ? element.MakeArrayType()
                : element.MakeArrayType(runtimeType.GetArrayRank());
        }

        if (runtimeType.IsGenericParameter)
        {
            if (runtimeType.DeclaringMethod is not null)
            {
                var position = runtimeType.GenericParameterPosition;
                if (position >= 0 && position < methodRuntimeArguments.Length)
                    return methodRuntimeArguments[position];
            }
            else if (runtimeType.DeclaringType is not null && typeRuntimeArguments is { Length: > 0 })
            {
                var mapped = SubstituteTypeParameterFromDeclaringType(runtimeType, runtimeType.DeclaringType, typeRuntimeArguments, methodRuntimeArguments);
                if (mapped is not null)
                    return mapped;
            }

            return runtimeType;
        }

        if (runtimeType.IsGenericType)
        {
            var substitutedArguments = runtimeType.GetGenericArguments()
                .Select(argument => SubstituteRuntimeType(argument, methodRuntimeArguments, typeRuntimeArguments))
                .ToArray();

            var definition = runtimeType.IsGenericTypeDefinition
                ? runtimeType
                : runtimeType.GetGenericTypeDefinition();

            return MakeGenericTypePreservingBuilders(definition, substitutedArguments);
        }

        return runtimeType;
    }

    private static Type? SubstituteTypeParameterFromDeclaringType(Type genericParameter, Type declaringType, Type[] typeRuntimeArguments, Type[] methodRuntimeArguments)
    {
        var definition = declaringType.IsGenericTypeDefinition
            ? declaringType
            : declaringType.GetGenericTypeDefinition();

        var definitionParameters = definition.GetGenericArguments();
        var index = Array.IndexOf(definitionParameters, genericParameter);
        if (index >= 0 && index < typeRuntimeArguments.Length)
        {
            var mapped = typeRuntimeArguments[index];
            if (!ReferenceEquals(mapped, genericParameter))
                return SubstituteRuntimeType(mapped, methodRuntimeArguments, typeRuntimeArguments);
        }

        // Some runtimes reuse declaring-type parameters for nested generic parameter builders where GenericParameterPosition
        // refers to the overall argument list instead of the declaring definition. Fall back to positional lookup if available.
        var position = genericParameter.GenericParameterPosition;
        if (position >= 0 && position < typeRuntimeArguments.Length)
        {
            var mapped = typeRuntimeArguments[position];
            if (!ReferenceEquals(mapped, genericParameter))
                return SubstituteRuntimeType(mapped, methodRuntimeArguments, typeRuntimeArguments);
        }

        return null;
    }

    private static Type MakeGenericTypePreservingBuilders(Type definition, Type[] arguments)
        => definition.MakeGenericType(arguments);

    private static Type GetProjectedRuntimeType(ConstructedMethodSymbol construction,
        ITypeSymbol symbol,
        CodeGenerator codeGen,
        bool treatUnitAsVoid,
        bool isTopLevel = true,
        HashSet<ITypeSymbol>? visiting = null)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));
        if (codeGen is null)
            throw new ArgumentNullException(nameof(codeGen));

        visiting ??= new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance);
        if (!visiting.Add(symbol))
        {
            if (symbol is ITypeParameterSymbol cyclicTypeParameter &&
                codeGen.TryGetRuntimeTypeForTypeParameter(cyclicTypeParameter, out var cyclicRuntimeType))
            {
                return cyclicRuntimeType;
            }

            throw new InvalidOperationException($"Detected cyclic type substitution while projecting runtime type for '{symbol}'.");
        }

        try
        {
            if (symbol is ITypeParameterSymbol typeParameter)
            {
                if (construction.TryGetTypeSubstitution(typeParameter, out var substitution))
                {
                    if (!ReferenceEquals(substitution, typeParameter))
                        return GetProjectedRuntimeType(construction, substitution, codeGen, treatUnitAsVoid, isTopLevel, visiting);
                }

                if (codeGen.TryResolveRuntimeTypeParameter(typeParameter, RuntimeTypeUsage.MethodBody, out var runtimeType))
                    return runtimeType;

                if (typeParameter.OwnerKind == TypeParameterOwnerKind.Method &&
                    typeParameter.DeclaringMethodParameterOwner is IMethodSymbol declaringMethod &&
                    TryGetMethodGenericParameter(declaringMethod, typeParameter.Ordinal, codeGen, out var methodGenericParameter))
                {
                    return methodGenericParameter;
                }

                throw new InvalidOperationException($"Unable to resolve runtime type for type parameter '{typeParameter.Name}'.");
            }

            if (symbol is RefTypeSymbol refType)
            {
                var elementType = GetProjectedRuntimeType(construction, refType.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false, visiting);
                return elementType.MakeByRefType();
            }

            if (symbol is IArrayTypeSymbol array)
            {
                var elementType = GetProjectedRuntimeType(construction, array.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false, visiting);
                return array.Rank == 1
                    ? elementType.MakeArrayType()
                    : elementType.MakeArrayType(array.Rank);
            }

            if (symbol is NullableTypeSymbol nullable)
            {
                var underlying = GetProjectedRuntimeType(construction, nullable.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false, visiting);
                return nullable.UnderlyingType.IsValueType
                    ? typeof(Nullable<>).MakeGenericType(underlying)
                    : underlying;
            }

            if (symbol is LiteralTypeSymbol literal)
                return GetProjectedRuntimeType(construction, literal.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false, visiting);

            if (symbol is ITupleTypeSymbol tuple)
            {
                var elementClrTypes = tuple.TupleElements
                    .Select(element => GetProjectedRuntimeType(construction, element.Type, codeGen, treatUnitAsVoid, isTopLevel: false, visiting))
                    .ToArray();

                return TypeSymbolExtensionsForCodeGen.GetValueTupleClrType(elementClrTypes, codeGen.Compilation);
            }

            if (symbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
            {
                var definition = named.ConstructedFrom as INamedTypeSymbol;
                Type runtimeDefinition;

                if (definition is not null && !SymbolEqualityComparer.Default.Equals(definition, named))
                {
                    runtimeDefinition = GetProjectedRuntimeType(construction, definition, codeGen, treatUnitAsVoid, isTopLevel: false, visiting);
                }
                else
                {
                    runtimeDefinition = treatUnitAsVoid && isTopLevel
                        ? TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(named, codeGen)
                        : TypeSymbolExtensionsForCodeGen.GetClrType(named, codeGen);
                }

                if (!runtimeDefinition.IsGenericTypeDefinition && !runtimeDefinition.ContainsGenericParameters)
                    return runtimeDefinition;

                if (!runtimeDefinition.IsGenericTypeDefinition)
                    return runtimeDefinition;

                var runtimeArguments = TypeSymbolExtensionsForCodeGen.GetRuntimeTypeArguments(named, runtimeDefinition)
                    .Select(argument => GetProjectedRuntimeType(construction, argument, codeGen, treatUnitAsVoid, isTopLevel: false, visiting))
                    .ToArray();

                return runtimeDefinition.MakeGenericType(runtimeArguments);
            }

            return treatUnitAsVoid && isTopLevel
                ? TypeSymbolExtensionsForCodeGen.GetClrTypeTreatingUnitAsVoid(symbol, codeGen)
                : TypeSymbolExtensionsForCodeGen.GetClrType(symbol, codeGen);
        }
        finally
        {
            visiting.Remove(symbol);
        }
    }

    private static bool TryMapStateMachineTypeParameter(
        ITypeParameterSymbol typeParameter,
        out ITypeParameterSymbol asyncMethodTypeParameter)
    {
        asyncMethodTypeParameter = null!;

        if (typeParameter.ContainingType is SynthesizedAsyncStateMachineTypeSymbol directStateMachine &&
            directStateMachine.TryMapToAsyncMethodTypeParameter(typeParameter, out asyncMethodTypeParameter))
        {
            return true;
        }

        if (typeParameter.ContainingType is SynthesizedAsyncStateMachineTypeSymbol directByOrdinal)
        {
            var ordinal = typeParameter.Ordinal;
            if ((uint)ordinal < (uint)directByOrdinal.TypeParameters.Length &&
                directByOrdinal.TypeParameters[ordinal] is ITypeParameterSymbol stateTypeParameter &&
                directByOrdinal.TryMapToAsyncMethodTypeParameter(stateTypeParameter, out asyncMethodTypeParameter))
            {
                return true;
            }
        }

        if (typeParameter.ContainingType is ConstructedNamedTypeSymbol constructedStateMachine &&
            constructedStateMachine.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol synthesizedStateMachine)
        {
            var ordinal = typeParameter.Ordinal;
            if ((uint)ordinal < (uint)synthesizedStateMachine.TypeParameters.Length &&
                synthesizedStateMachine.TypeParameters[ordinal] is ITypeParameterSymbol synthesizedTypeParameter &&
                synthesizedStateMachine.TryMapToAsyncMethodTypeParameter(synthesizedTypeParameter, out asyncMethodTypeParameter))
            {
                return true;
            }
        }

        if (typeParameter.OriginalDefinition is ITypeParameterSymbol original &&
            !ReferenceEquals(original, typeParameter))
        {
            return TryMapStateMachineTypeParameter(original, out asyncMethodTypeParameter);
        }

        return false;
    }
}

internal static class ConstructedMethodDebugging
{
    public static bool IsEnabled()
    {
        var value = Environment.GetEnvironmentVariable("RAVEN_DEBUG_CONSTRUCTED_METHOD");
        return string.Equals(value, "1", StringComparison.Ordinal);
    }
}
