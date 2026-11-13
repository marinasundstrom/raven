using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal static class PEAttributeDataFactory
{
    public static AttributeData? Create(
        ReflectionAttributeTypeProvider reflectionTypeLoader,
        CustomAttributeData attribute)
    {
        var attributeClass = attribute.AttributeType.FullName is { } metadataName
            ? reflectionTypeLoader.GetTypeByMetadataName(metadataName)
            : null;
        if (attributeClass is null or { TypeKind: TypeKind.Error })
            attributeClass = reflectionTypeLoader.ResolveType(attribute.AttributeType) as NamedTypeSymbol;

        if (attributeClass is null or { TypeKind: TypeKind.Error })
            return null;

        // The metadata signature identifies the overload. Boxed argument values
        // can have a more specific runtime type than the declared parameter.
        var parameterTypes = attribute.Constructor.GetParameters()
            .Select(parameter => reflectionTypeLoader.ResolveType(parameter.ParameterType)).ToArray();
        var attributeConstructor = attributeClass.GetMembers(".ctor")
            .OfType<MethodSymbol>()
            .FirstOrDefault(ctor => MatchesSignature(ctor, parameterTypes));

        if (attributeConstructor is null)
            return null;

        var constructorArguments = ImmutableArray.CreateRange(
            attribute.ConstructorArguments.Select(argument => CreateTypedConstant(reflectionTypeLoader, argument)));
        var namedArguments = ImmutableArray.CreateRange(
            attribute.NamedArguments.Select(named =>
                new KeyValuePair<string, TypedConstant>(
                    named.MemberName,
                    CreateTypedConstant(reflectionTypeLoader, named.TypedValue))));

        return new AttributeData(
            attributeClass,
            attributeConstructor,
            constructorArguments,
            namedArguments,
            applicationSyntaxReference: null);
    }

    private static bool MatchesSignature(MethodSymbol constructor, TypeSymbol?[] parameterTypes)
    {
        var parameters = constructor.Parameters;
        if (parameters.Length != parameterTypes.Length)
            return false;

        for (var i = 0; i < parameters.Length; i++)
        {
            if (parameterTypes[i] is not { } parameterType ||
                !SymbolEqualityComparer.Default.Equals(parameters[i].Type, parameterType))
                return false;
        }
        return true;
    }

    private static TypedConstant CreateTypedConstant(
        ReflectionAttributeTypeProvider reflectionTypeLoader,
        CustomAttributeTypedArgument argument)
    {
        var type = reflectionTypeLoader.ResolveType(argument.ArgumentType);

        if (argument.Value is null)
            return TypedConstant.CreateNull(type);

        if (argument.ArgumentType.IsArray &&
            argument.Value is IReadOnlyCollection<CustomAttributeTypedArgument> elements)
        {
            var values = ImmutableArray.CreateRange(
                elements.Select(element => CreateTypedConstant(reflectionTypeLoader, element)));
            return TypedConstant.CreateArray(type, values);
        }

        if (argument.Value is Type typeValue &&
            reflectionTypeLoader.ResolveType(typeValue) is TypeSymbol resolvedType)
        {
            return TypedConstant.CreateType(type, resolvedType);
        }

        return argument.ArgumentType.IsEnum
            ? TypedConstant.CreateEnum(type, argument.Value)
            : TypedConstant.CreatePrimitive(type, argument.Value);
    }
}
