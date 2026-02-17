using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal static class PEReflectionTypeIdentity
{
    public static bool AreEquivalent(Type? left, Type? right)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (left is null || right is null)
            return false;

        if (left.IsByRef || right.IsByRef)
        {
            if (!(left.IsByRef && right.IsByRef))
                return false;

            return AreEquivalent(left.GetElementType(), right.GetElementType());
        }

        if (left.IsPointer || right.IsPointer)
        {
            if (!(left.IsPointer && right.IsPointer))
                return false;

            return AreEquivalent(left.GetElementType(), right.GetElementType());
        }

        if (left.IsArray || right.IsArray)
        {
            if (!(left.IsArray && right.IsArray))
                return false;

            if (left.GetArrayRank() != right.GetArrayRank())
                return false;

            return AreEquivalent(left.GetElementType(), right.GetElementType());
        }

        if (left.IsGenericParameter || right.IsGenericParameter)
            return AreEquivalentGenericParameter(left, right);

        if (left.IsGenericType || right.IsGenericType)
            return AreEquivalentGenericType(left, right);

        var leftMetadataName = GetMetadataName(left);
        var rightMetadataName = GetMetadataName(right);

        if (!string.Equals(leftMetadataName, rightMetadataName, StringComparison.Ordinal))
            return false;

        return string.Equals(
            left.Assembly.GetName().Name,
            right.Assembly.GetName().Name,
            StringComparison.OrdinalIgnoreCase);
    }

    private static bool AreEquivalentGenericParameter(Type left, Type right)
    {
        if (!(left.IsGenericParameter && right.IsGenericParameter))
            return false;

        if (left.GenericParameterPosition != right.GenericParameterPosition)
            return false;

        var leftDeclaringMethod = left.DeclaringMethod;
        var rightDeclaringMethod = right.DeclaringMethod;

        if (leftDeclaringMethod is not null || rightDeclaringMethod is not null)
        {
            if (leftDeclaringMethod is null || rightDeclaringMethod is null)
                return false;

            return string.Equals(leftDeclaringMethod.Name, rightDeclaringMethod.Name, StringComparison.Ordinal) &&
                   AreEquivalent(leftDeclaringMethod.DeclaringType, rightDeclaringMethod.DeclaringType);
        }

        return AreEquivalent(left.DeclaringType, right.DeclaringType);
    }

    private static bool AreEquivalentGenericType(Type left, Type right)
    {
        if (!(left.IsGenericType && right.IsGenericType))
            return false;

        var leftDefinition = left.IsGenericTypeDefinition ? left : left.GetGenericTypeDefinition();
        var rightDefinition = right.IsGenericTypeDefinition ? right : right.GetGenericTypeDefinition();

        if (!AreEquivalent(leftDefinition, rightDefinition))
            return false;

        var leftArguments = left.GetGenericArguments();
        var rightArguments = right.GetGenericArguments();

        if (leftArguments.Length != rightArguments.Length)
            return false;

        for (var i = 0; i < leftArguments.Length; i++)
        {
            if (!AreEquivalent(leftArguments[i], rightArguments[i]))
                return false;
        }

        return true;
    }

    private static string GetMetadataName(Type type)
    {
        if (type.DeclaringType is { } declaringType)
        {
            var declaringName = GetMetadataName(declaringType);
            return $"{declaringName}+{type.Name}";
        }

        return type.FullName ?? type.Name;
    }
}
