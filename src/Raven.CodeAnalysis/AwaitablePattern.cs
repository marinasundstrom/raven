using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal enum AwaitablePatternFailure
{
    None,
    GetAwaiterMissing,
    AwaiterTypeError,
    IsCompletedMissing,
    GetResultMissing,
}

internal readonly struct AwaitablePatternInfo
{
    public AwaitablePatternInfo(
        IMethodSymbol getAwaiterMethod,
        IPropertySymbol isCompletedProperty,
        IMethodSymbol getResultMethod)
    {
        GetAwaiterMethod = getAwaiterMethod ?? throw new ArgumentNullException(nameof(getAwaiterMethod));
        IsCompletedProperty = isCompletedProperty ?? throw new ArgumentNullException(nameof(isCompletedProperty));
        GetResultMethod = getResultMethod ?? throw new ArgumentNullException(nameof(getResultMethod));
        AwaiterType = getAwaiterMethod.ReturnType ?? throw new ArgumentNullException(nameof(getAwaiterMethod.ReturnType));
    }

    public IMethodSymbol GetAwaiterMethod { get; }

    public IPropertySymbol IsCompletedProperty { get; }

    public IMethodSymbol GetResultMethod { get; }

    public ITypeSymbol AwaiterType { get; }
}

internal static class AwaitablePattern
{
    public static bool TryFind(
        ITypeSymbol type,
        Func<ISymbol, bool>? isAccessible,
        out AwaitablePatternInfo awaitable,
        out AwaitablePatternFailure failure,
        out ITypeSymbol? awaiterType)
    {
        awaitable = default;
        failure = AwaitablePatternFailure.None;
        awaiterType = null;

        if (type is null || type.TypeKind == TypeKind.Error)
        {
            failure = AwaitablePatternFailure.AwaiterTypeError;
            return false;
        }

        var getAwaiter = type
            .GetMembers("GetAwaiter")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => !m.IsStatic && m.Parameters.Length == 0 && IsAllowed(m, isAccessible));

        if (getAwaiter is null)
        {
            failure = AwaitablePatternFailure.GetAwaiterMissing;
            return false;
        }

        awaiterType = getAwaiter.ReturnType;
        if (awaiterType is null || awaiterType.TypeKind == TypeKind.Error)
        {
            failure = AwaitablePatternFailure.AwaiterTypeError;
            return false;
        }

        var isCompleted = awaiterType
            .GetMembers("IsCompleted")
            .OfType<IPropertySymbol>()
            .FirstOrDefault(p => !p.IsStatic
                && p.Type.SpecialType == SpecialType.System_Boolean
                && IsAllowed(p, isAccessible));

        if (isCompleted is null)
        {
            failure = AwaitablePatternFailure.IsCompletedMissing;
            return false;
        }

        var getResult = awaiterType
            .GetMembers("GetResult")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => !m.IsStatic && m.Parameters.Length == 0 && IsAllowed(m, isAccessible));

        if (getResult is null)
        {
            failure = AwaitablePatternFailure.GetResultMissing;
            return false;
        }

        awaitable = new AwaitablePatternInfo(getAwaiter, isCompleted, getResult);
        return true;
    }

    private static bool IsAllowed(ISymbol member, Func<ISymbol, bool>? isAccessible)
    {
        if (isAccessible is null)
            return true;

        return isAccessible(member);
    }
}
