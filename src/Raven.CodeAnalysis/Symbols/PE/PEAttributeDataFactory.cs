using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal static class PEAttributeDataFactory
{
    public static AttributeData? Create(
        ReflectionTypeLoader reflectionTypeLoader,
        CustomAttributeData attribute)
    {
        var attributeClass = attribute.AttributeType.FullName is { } metadataName
            ? reflectionTypeLoader.Compilation.GetTypeByMetadataName(metadataName)
            : null;
        if (attributeClass is null or { TypeKind: TypeKind.Error })
            attributeClass = reflectionTypeLoader.ResolveType(attribute.AttributeType) as INamedTypeSymbol;

        if (attributeClass is null or { TypeKind: TypeKind.Error })
            return null;

        var attributeConstructor = attributeClass.GetMembers(".ctor")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(ctor => ctor.Parameters.Length == attribute.ConstructorArguments.Count)
            ?? attributeClass.GetMembers(".ctor").OfType<IMethodSymbol>().FirstOrDefault();

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

    private static TypedConstant CreateTypedConstant(
        ReflectionTypeLoader reflectionTypeLoader,
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
            reflectionTypeLoader.ResolveType(typeValue) is ITypeSymbol resolvedType)
        {
            return TypedConstant.CreateType(type, resolvedType);
        }

        return TypedConstant.CreatePrimitive(type, argument.Value);
    }
}
