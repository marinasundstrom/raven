using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class UseDisposalUtilities
{
    public static bool SupportsUseDisposal(
        Compilation compilation,
        ITypeSymbol type,
        bool preferAsync)
    {
        return TryResolveUseDisposeMethod(compilation, type, preferAsync, out _, out _);
    }

    public static bool TryResolveUseDisposeMethod(
        Compilation compilation,
        ITypeSymbol type,
        bool preferAsync,
        out IMethodSymbol? disposeMethod,
        out bool useAwait)
    {
        disposeMethod = null;
        useAwait = false;

        if (type.TypeKind == TypeKind.Error)
            return false;

        if (preferAsync &&
            TryGetInterfaceMethod(compilation, type, "System.IAsyncDisposable", "DisposeAsync", out var asyncDisposeMethod) &&
            asyncDisposeMethod is not null &&
            AwaitablePattern.TryFind(asyncDisposeMethod.ReturnType, isAccessible: null, out _, out _, out _))
        {
            disposeMethod = asyncDisposeMethod;
            useAwait = true;
            return true;
        }

        if (TryGetDisposableMethod(compilation, type, out var syncDisposeMethod))
        {
            disposeMethod = syncDisposeMethod;
            return true;
        }

        return false;
    }

    public static string GetExpectedUseTargetDisplay(Compilation compilation, bool preferAsync)
    {
        if (!preferAsync)
            return GetDisposableDisplay(compilation);

        var parts = new List<string>();

        if (TryGetNamedType(compilation, "System.IAsyncDisposable", out var asyncDisposable))
            parts.Add(asyncDisposable.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var disposableDisplay = GetDisposableDisplay(compilation);
        if (!string.IsNullOrEmpty(disposableDisplay))
            parts.Add(disposableDisplay);

        return parts.Count switch
        {
            0 => "IDisposable",
            1 => parts[0],
            _ => string.Join(" or ", parts.Distinct(StringComparer.Ordinal))
        };
    }

    public static BoundExpression CreateDisposeInvocationExpression(
        Compilation compilation,
        BoundExpression receiver,
        IMethodSymbol disposeMethod,
        bool useAwait)
    {
        var invocation = new BoundInvocationExpression(
            disposeMethod,
            Array.Empty<BoundExpression>(),
            receiver);

        if (!useAwait)
            return invocation;

        if (!AwaitablePattern.TryFind(disposeMethod.ReturnType, isAccessible: null, out var awaitable, out _, out _))
            return invocation;

        var resultType = awaitable.GetResultMethod.ReturnType;
        if (resultType.SpecialType == SpecialType.System_Void)
            resultType = compilation.GetSpecialType(SpecialType.System_Unit);

        return new BoundAwaitExpression(
            invocation,
            resultType,
            awaitable.AwaiterType,
            awaitable.GetAwaiterMethod,
            awaitable.GetResultMethod,
            awaitable.IsCompletedProperty);
    }

    public static BoundExpression CreateBlockingDisposeInvocationExpression(
        Compilation compilation,
        BoundExpression receiver,
        IMethodSymbol disposeMethod,
        bool useAwait)
    {
        var invocation = new BoundInvocationExpression(
            disposeMethod,
            Array.Empty<BoundExpression>(),
            receiver,
            requiresReceiverAddress: receiver.Type?.IsValueType == true);

        if (!useAwait)
            return invocation;

        if (!AwaitablePattern.TryFind(disposeMethod.ReturnType, isAccessible: null, out var awaitable, out _, out _))
            return invocation;

        var getAwaiterInvocation = new BoundInvocationExpression(
            awaitable.GetAwaiterMethod,
            Array.Empty<BoundExpression>(),
            invocation,
            requiresReceiverAddress: invocation.Type?.IsValueType == true);

        return new BoundInvocationExpression(
            awaitable.GetResultMethod,
            Array.Empty<BoundExpression>(),
            getAwaiterInvocation,
            requiresReceiverAddress: awaitable.AwaiterType.IsValueType);
    }

    private static bool TryGetDisposableMethod(
        Compilation compilation,
        ITypeSymbol type,
        out IMethodSymbol? disposeMethod)
    {
        return TryGetInterfaceMethod(compilation, type, "System.IDisposable", nameof(IDisposable.Dispose), out disposeMethod);
    }

    private static bool TryGetInterfaceMethod(
        Compilation compilation,
        ITypeSymbol type,
        string metadataName,
        string methodName,
        out IMethodSymbol? method)
    {
        method = null;

        if (!TryGetNamedType(compilation, metadataName, out var interfaceType))
            return false;

        if (!ImplementsOrEquals(type, interfaceType))
            return false;

        method = interfaceType
            .GetMembers(methodName)
            .OfType<IMethodSymbol>()
            .FirstOrDefault(static member => !member.IsStatic && member.Parameters.Length == 0);

        return method is not null;
    }

    private static bool TryGetNamedType(
        Compilation compilation,
        string metadataName,
        out INamedTypeSymbol namedType)
    {
        namedType = compilation.GetTypeByMetadataName(metadataName) as INamedTypeSymbol ?? null!;
        return namedType is not null && namedType.TypeKind != TypeKind.Error;
    }

    private static bool ImplementsOrEquals(ITypeSymbol type, INamedTypeSymbol interfaceType)
    {
        if (type is INamedTypeSymbol named &&
            SymbolEqualityComparer.Default.Equals(named, interfaceType))
        {
            return true;
        }

        return SemanticFacts.ImplementsInterface(type, interfaceType, SymbolEqualityComparer.Default);
    }

    private static string GetDisposableDisplay(Compilation compilation)
    {
        var disposableType = compilation.GetSpecialType(SpecialType.System_IDisposable);
        return disposableType.TypeKind == TypeKind.Error
            ? "IDisposable"
            : disposableType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }
}
