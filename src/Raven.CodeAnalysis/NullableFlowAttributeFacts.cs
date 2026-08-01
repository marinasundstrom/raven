using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal static class NullableFlowAttributeFacts
{
    private const string AttributeNamespace = "System.Diagnostics.CodeAnalysis";

    public static bool ReturnMayBeNull(IMethodSymbol method)
        => HasAttribute(method.GetReturnTypeAttributes(), "MaybeNullAttribute");

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
