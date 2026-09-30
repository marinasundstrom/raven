using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

// Reflection construction belongs to the .NET backend. Semantic symbols supply
// definitions and substitutions; runtime parameter caches belong to this emit.
internal static class ConstructedTypeCodeGenResolver
{
    internal static System.Reflection.TypeInfo GetTypeInfo(ConstructedNamedTypeSymbol symbol, CodeGenerator codeGen)
    {
        var runtimeArguments = symbol.GetAllTypeArguments();

        if (symbol.ConstructedFrom is PENamedTypeSymbol pen)
        {
            var genericTypeDef = TypeSymbolExtensionsForCodeGen.GetClrType(pen, codeGen);
            if (runtimeArguments.IsDefaultOrEmpty)
                return genericTypeDef.GetTypeInfo();

            var resolved = runtimeArguments
                .Select(arg => ResolveRuntimeTypeArgument(symbol, arg, codeGen))
                .ToArray();
            return genericTypeDef.MakeGenericType(resolved).GetTypeInfo();
        }

        if (symbol.ConstructedFrom is SourceNamedTypeSymbol source)
        {
            var definitionType = codeGen.GetTypeBuilder(source) ?? throw new InvalidOperationException("Missing type builder for generic definition.");
            if (source.IsExtensionDeclaration)
                return definitionType.GetTypeInfo();
            if (runtimeArguments.IsDefaultOrEmpty)
                return definitionType.GetTypeInfo();

            var runtimeArgs = runtimeArguments
                .Select(arg => ResolveRuntimeTypeArgument(symbol, arg, codeGen))
                .ToArray();
            try
            {
                var constructed = definitionType.MakeGenericType(runtimeArgs);
                return constructed.GetTypeInfo();
            }
            catch (InvalidOperationException exception)
            {
                throw new InvalidOperationException(
                    $"Unable to construct runtime type '{source.ToFullyQualifiedMetadataName()}' " +
                    $"from builder '{definitionType}' with {runtimeArgs.Length} type argument(s).",
                    exception);
            }
        }

        throw new InvalidOperationException("ConstructedNamedTypeSymbol is not based on a supported symbol type.");
    }

    private static Type ResolveRuntimeTypeArgument(ConstructedNamedTypeSymbol symbol, ITypeSymbol typeArgument, CodeGenerator codeGen)
    {
        if (typeArgument is ITypeParameterSymbol { OwnerKind: TypeParameterOwnerKind.Method } methodTypeParameter &&
            methodTypeParameter.DeclaringMethodParameterOwner is IMethodSymbol methodSymbol)
        {
            if (codeGen.TryResolveRuntimeTypeParameter(methodTypeParameter, RuntimeTypeUsage.MethodBody, out var methodBodyResolved))
                return methodBodyResolved;

            if (TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var methodParameter))
                return methodParameter;

            if (codeGen.TryGetRuntimeTypeForTypeParameter(methodTypeParameter, out var resolved))
            {
                if (IsMethodGenericParameter(resolved))
                    return resolved;

                if (TryGetMethodGenericParameter(methodSymbol, methodTypeParameter.Ordinal, codeGen, out var refreshedMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(methodTypeParameter, refreshedMethodParameter);
                    return refreshedMethodParameter;
                }

                if (symbol.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
                    TryGetMethodGenericParameter(stateMachine.AsyncMethod, methodTypeParameter.Ordinal, codeGen, out var asyncMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(methodTypeParameter, asyncMethodParameter);
                    return asyncMethodParameter;
                }

                return resolved;
            }

            if (symbol.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol fallbackStateMachine &&
                TryGetMethodGenericParameter(fallbackStateMachine.AsyncMethod, methodTypeParameter.Ordinal, codeGen, out var mappedFallback))
            {
                codeGen.CacheRuntimeTypeParameter(methodTypeParameter, mappedFallback);
                return mappedFallback;
            }
        }

        if (typeArgument is ITypeParameterSymbol typeParameter)
        {
            if (codeGen.TryResolveRuntimeTypeParameter(typeParameter, RuntimeTypeUsage.MethodBody, out var methodBodyResolved))
                return methodBodyResolved;

            if (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameter, out var runtimeType))
                return runtimeType;

            if (TryGetMappedAsyncParameter(typeParameter, out var stateMachine, out var asyncParameter) &&
                stateMachine is not null &&
                asyncParameter is not null)
            {
                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var asyncMethodParameter))
                {
                    codeGen.CacheRuntimeTypeParameter(asyncParameter, asyncMethodParameter);
                    return asyncMethodParameter;
                }

                if (codeGen.TryGetRuntimeTypeForTypeParameter(asyncParameter, out var asyncResolved))
                {
                    if (IsMethodGenericParameter(asyncResolved))
                        return asyncResolved;

                    if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var refreshedAsyncMethodParameter))
                    {
                        codeGen.CacheRuntimeTypeParameter(asyncParameter, refreshedAsyncMethodParameter);
                        return refreshedAsyncMethodParameter;
                    }

                    throw new InvalidOperationException("Unable to map async method type parameter to runtime generic parameter.");
                }

                if (TryGetMethodGenericParameter(stateMachine.AsyncMethod, asyncParameter.Ordinal, codeGen, out var mappedFallback))
                {
                    codeGen.CacheRuntimeTypeParameter(asyncParameter, mappedFallback);
                    return mappedFallback;
                }

                throw new InvalidOperationException("Unable to resolve async method generic parameter for state machine mapping.");
            }
            throw new InvalidOperationException("Unable to map state machine type parameter to async method generic parameter.");
        }

        return TypeSymbolExtensionsForCodeGen.GetClrType(typeArgument, codeGen);
    }

    private static bool TryGetMappedAsyncParameter(
        ITypeParameterSymbol typeParameter,
        out SynthesizedAsyncStateMachineTypeSymbol? stateMachine,
        out ITypeParameterSymbol? asyncParameter)
    {
        stateMachine = null;
        asyncParameter = null;

        var containingType = typeParameter.ContainingType;
        if (containingType is SynthesizedAsyncStateMachineTypeSymbol direct &&
            direct.TryMapToAsyncMethodTypeParameter(typeParameter, out var mapped))
        {
            stateMachine = direct;
            asyncParameter = mapped;
            return true;
        }

        if (containingType is ConstructedNamedTypeSymbol constructed &&
            constructed.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol constructedStateMachine &&
            typeParameter.OriginalDefinition is ITypeParameterSymbol original &&
            constructedStateMachine.TryMapToAsyncMethodTypeParameter(original, out mapped))
        {
            stateMachine = constructedStateMachine;
            asyncParameter = mapped;
            return true;
        }

        return false;
    }

    private static bool TryGetMethodGenericParameter(IMethodSymbol methodSymbol, int ordinal, CodeGenerator codeGen, out Type parameter)
    {
        if (methodSymbol is null)
            throw new ArgumentNullException(nameof(methodSymbol));

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

    private static bool IsMethodGenericParameter(Type runtimeType)
    {
        try
        {
            return runtimeType.IsGenericParameter && runtimeType.IsGenericMethodParameter;
        }
        catch (NotSupportedException)
        {
            return false;
        }
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

}
