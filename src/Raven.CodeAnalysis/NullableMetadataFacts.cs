using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal static class NullableMetadataFacts
{
    private const string AttributeNamespace = "System.Diagnostics.CodeAnalysis";

    public static ITypeSymbol GetReturnType(IMethodSymbol method)
    {
        var declaredType = method.ReturnType;
        if (declaredType.IsValueType || !HasAttribute(method.GetReturnTypeAttributes(), "MaybeNullAttribute"))
            return declaredType;

        return declaredType.GetNullableType();
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
        => SymbolAllowsNullInput(symbol, declaredType)
            ? declaredType.GetNullableType()
            : declaredType.GetNonNullableType();

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
