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
        MethodSymbol getAwaiterMethod,
        PropertySymbol isCompletedProperty,
        MethodSymbol getResultMethod)
    {
        GetAwaiterMethod = getAwaiterMethod ?? throw new ArgumentNullException(nameof(getAwaiterMethod));
        IsCompletedProperty = isCompletedProperty ?? throw new ArgumentNullException(nameof(isCompletedProperty));
        GetResultMethod = getResultMethod ?? throw new ArgumentNullException(nameof(getResultMethod));
        AwaiterType = getAwaiterMethod.ReturnType ?? throw new ArgumentNullException(nameof(getAwaiterMethod.ReturnType));
    }

    public MethodSymbol GetAwaiterMethod { get; }

    public PropertySymbol IsCompletedProperty { get; }

    public MethodSymbol GetResultMethod { get; }

    public TypeSymbol AwaiterType { get; }
}

internal static class AwaitablePattern
{
    public static bool TryFind(
        TypeSymbol type,
        Func<Symbol, bool>? isAccessible,
        out AwaitablePatternInfo awaitable,
        out AwaitablePatternFailure failure,
        out TypeSymbol? awaiterType)
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
            .OfType<MethodSymbol>()
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
            .OfType<PropertySymbol>()
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
            .OfType<MethodSymbol>()
            .FirstOrDefault(m => !m.IsStatic && m.Parameters.Length == 0 && IsAllowed(m, isAccessible));

        if (getResult is null)
        {
            failure = AwaitablePatternFailure.GetResultMissing;
            return false;
        }

        awaitable = new AwaitablePatternInfo(getAwaiter, isCompleted, getResult);
        return true;
    }

    private static bool IsAllowed(Symbol member, Func<Symbol, bool>? isAccessible)
    {
        if (isAccessible is null)
            return true;

        return isAccessible(member);
    }
}
