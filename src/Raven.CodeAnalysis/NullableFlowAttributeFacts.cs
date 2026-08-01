using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal static class NullableFlowAttributeFacts
{
    private const string AttributeNamespace = "System.Diagnostics.CodeAnalysis";

    public static bool ReturnMayBeNull(IMethodSymbol method)
        => HasAttribute(method.GetReturnTypeAttributes(), "MaybeNullAttribute");

    public static bool TryGetNotNullIfNotNull(IMethodSymbol method, out string parameterName)
    {
        foreach (var attribute in method.GetReturnTypeAttributes())
        {
            if (!IsAttribute(attribute, "NotNullIfNotNullAttribute") ||
                attribute.ConstructorArguments is not [{ Value: string value }])
            {
                continue;
            }

            parameterName = value;
            return true;
        }

        parameterName = string.Empty;
        return false;
    }

    public static bool TryGetNotNullWhen(IParameterSymbol parameter, out bool returnValue)
    {
        foreach (var attribute in parameter.GetAttributes())
        {
            if (!IsAttribute(attribute, "NotNullWhenAttribute") ||
                attribute.ConstructorArguments is not [{ Value: bool value }])
            {
                continue;
            }

            returnValue = value;
            return true;
        }

        returnValue = false;
        return false;
    }

    public static bool TryGetMaybeNullWhen(IParameterSymbol parameter, out bool returnValue)
    {
        foreach (var attribute in parameter.GetAttributes())
        {
            if (!IsAttribute(attribute, "MaybeNullWhenAttribute") ||
                attribute.ConstructorArguments is not [{ Value: bool value }])
            {
                continue;
            }

            returnValue = value;
            return true;
        }

        returnValue = false;
        return false;
    }

    public static bool ParameterIsNotNullAfterCall(IParameterSymbol parameter)
        => HasAttribute(parameter.GetAttributes(), "NotNullAttribute");

    public static bool ParameterMayBeNullAfterCall(IParameterSymbol parameter)
        => HasAttribute(parameter.GetAttributes(), "MaybeNullAttribute");

    public static ImmutableArray<string> GetNotNullMembers(IMethodSymbol method)
    {
        foreach (var attribute in method.GetAttributes())
        {
            if (IsAttribute(attribute, "MemberNotNullAttribute"))
                return GetMemberNames(attribute, argumentOffset: 0);
        }

        return ImmutableArray<string>.Empty;
    }

    public static bool TryGetNotNullMembersWhen(
        IMethodSymbol method,
        out bool returnValue,
        out ImmutableArray<string> memberNames)
    {
        foreach (var attribute in method.GetAttributes())
        {
            if (!IsAttribute(attribute, "MemberNotNullWhenAttribute") ||
                attribute.ConstructorArguments is not [{ Value: bool value }, ..])
            {
                continue;
            }

            returnValue = value;
            memberNames = GetMemberNames(attribute, argumentOffset: 1);
            return !memberNames.IsDefaultOrEmpty;
        }

        returnValue = false;
        memberNames = ImmutableArray<string>.Empty;
        return false;
    }

    public static bool ParameterAllowsNullInput(IParameterSymbol parameter)
        => SymbolAllowsNullInput(parameter, parameter.Type);

    public static bool SymbolAllowsNullInput(ISymbol symbol, ITypeSymbol declaredType)
    {
        if (HasInputAttribute(symbol, "DisallowNullAttribute"))
            return false;

        return HasInputAttribute(symbol, "AllowNullAttribute") || declaredType.IsNullable;
    }

    public static ITypeSymbol GetInputType(ISymbol symbol, ITypeSymbol declaredType)
        => declaredType.WithNullableAnnotation(
            SymbolAllowsNullInput(symbol, declaredType)
                ? NullableAnnotation.Annotated
                : NullableAnnotation.NotAnnotated);

    private static bool HasInputAttribute(ISymbol symbol, string name)
    {
        if (symbol is IPropertySymbol { SetMethod: { } setter } &&
            setter.Parameters.LastOrDefault() is { } valueParameter &&
            HasAttribute(valueParameter.GetAttributes(), name))
        {
            return true;
        }

        return HasAttribute(symbol.GetAttributes(), name);
    }

    private static ImmutableArray<string> GetMemberNames(AttributeData attribute, int argumentOffset)
    {
        var builder = ImmutableArray.CreateBuilder<string>();

        for (var i = argumentOffset; i < attribute.ConstructorArguments.Length; i++)
        {
            var argument = attribute.ConstructorArguments[i];
            if (argument.Value is string memberName)
            {
                builder.Add(memberName);
                continue;
            }

            foreach (var value in argument.Values)
            {
                if (value.Value is string arrayMemberName)
                    builder.Add(arrayMemberName);
            }
        }

        return builder.ToImmutable();
    }

    private static bool HasAttribute(ImmutableArray<AttributeData> attributes, string name)
        => attributes.Any(attribute => IsAttribute(attribute, name));

    private static bool IsAttribute(AttributeData attribute, string name)
        => attribute.AttributeClass is
        {
            Name: var attributeName,
            ContainingNamespace: { } containingNamespace
        } &&
        attributeName == name &&
        containingNamespace.ToMetadataName() == AttributeNamespace;
}
