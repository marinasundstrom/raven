using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class NullableMetadataFacts
{
    private const string AttributeNamespace = "System.Diagnostics.CodeAnalysis";

    public static TypeSymbol GetReturnType(MethodSymbol method)
    {
        var declaredType = method.ReturnType;
        if (declaredType is NullableTypeSymbol
            {
                AbiProjection: NullableAbiProjection.AnnotatedUnderlyingType,
                UnderlyingType.IsValueType: true
            } annotatedValue)
        {
            return annotatedValue.UnderlyingType;
        }

        if (declaredType.IsValueType || !HasAttribute(method.GetReturnTypeAttributes(), "MaybeNullAttribute"))
            return declaredType;

        return declaredType.GetNullableType();
    }

    public static bool ParameterAllowsNullInput(ParameterSymbol parameter)
        => SymbolAllowsNullInput(parameter, parameter.Type);

    public static bool SymbolAllowsNullInput(Symbol symbol, TypeSymbol declaredType)
    {
        if (HasInputAttribute(symbol, "DisallowNullAttribute"))
            return false;

        return HasInputAttribute(symbol, "AllowNullAttribute") || declaredType.IsNullable;
    }

    public static TypeSymbol GetInputType(Symbol symbol, TypeSymbol declaredType)
        => SymbolAllowsNullInput(symbol, declaredType)
            ? declaredType.GetNullableType()
            : declaredType.GetNonNullableType();

    private static bool HasInputAttribute(Symbol symbol, string name)
    {
        if (symbol is PropertySymbol { SetMethod: { } setter } &&
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
