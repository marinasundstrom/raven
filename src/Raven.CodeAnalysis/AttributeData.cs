using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public sealed class AttributeData
{
    internal AttributeData(
        NamedTypeSymbol attributeClass,
        MethodSymbol attributeConstructor,
        ImmutableArray<TypedConstant> constructorArguments,
        ImmutableArray<KeyValuePair<string, TypedConstant>> namedArguments,
        SyntaxReference? applicationSyntaxReference)
    {
        AttributeClass = attributeClass;
        AttributeConstructor = attributeConstructor;
        ConstructorArguments = constructorArguments;
        NamedArguments = namedArguments;
        ApplicationSyntaxReference = applicationSyntaxReference;
    }

    public NamedTypeSymbol AttributeClass { get; }

    public MethodSymbol AttributeConstructor { get; }

    public ImmutableArray<TypedConstant> ConstructorArguments { get; }

    public ImmutableArray<KeyValuePair<string, TypedConstant>> NamedArguments { get; }

    public SyntaxReference? ApplicationSyntaxReference { get; }
}
