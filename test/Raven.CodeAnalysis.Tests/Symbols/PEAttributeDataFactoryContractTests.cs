using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class PEAttributeDataFactoryContractTests
{
    [Fact]
    public void ResolvesPrimitiveArrayTypeNullAndNamedArguments()
    {
        var provider = new Provider();
        var attribute = Data();
        var owner = Assert.IsAssignableFrom<NamedTypeSymbol>(provider.ResolveType(attribute.AttributeType));
        var ctor = Constructor(owner, 4);
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(ctor));
        provider.MetadataType = owner;
        var data = PEAttributeDataFactory.Create(provider, attribute)!;
        Assert.Same(owner, data.AttributeClass);
        Assert.Same(ctor, data.AttributeConstructor);
        Assert.Null(data.ApplicationSyntaxReference);
        Assert.Equal(4, data.ConstructorArguments.Length);
        Assert.Equal(7, data.ConstructorArguments[0].Value);
        var array = data.ConstructorArguments[1];
        Assert.Equal(TypedConstantKind.Array, array.Kind);
        Assert.Equal(new object?[] { 3, 1 }, array.Values.Select(value => value.Value));
        Assert.Same(provider.ResolveType(typeof(int[])), array.Type);
        Assert.Equal(TypedConstantKind.Type, data.ConstructorArguments[2].Kind);
        Assert.Same(provider.ResolveType(typeof(string)), data.ConstructorArguments[2].Value);
        Assert.True(data.ConstructorArguments[3].IsNull);
        Assert.Equal("Label", Assert.Single(data.NamedArguments).Key);
        Assert.Equal("named", data.NamedArguments[0].Value.Value);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MissingOrErrorMetadataTypeFallsBackToReflection(bool error)
    {
        var provider = new Provider();
        var attribute = Data();
        var owner = (NamedTypeSymbol)provider.ResolveType(attribute.AttributeType)!;
        var ctor = Constructor(owner, 4);
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(ctor));
        provider.MetadataType = error ? Make<NamedTypeSymbol>(SymbolKind.Type, "Error", TypeKind.Error) : null;
        Assert.Same(owner, PEAttributeDataFactory.Create(provider, attribute)!.AttributeClass);
    }

    [Fact]
    public void UnresolvableAttributeAndMissingConstructorReturnNull()
    {
        var provider = new Provider { MissingType = typeof(FixtureAttribute) };
        Assert.Null(PEAttributeDataFactory.Create(provider, Data()));
        provider.MissingType = null;
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(FixtureAttribute))!;
        Set(owner, "GetMembers", ImmutableArray<Symbol>.Empty);
        Assert.Null(PEAttributeDataFactory.Create(provider, Data()));
    }

    [Fact]
    public void SelectsMatchingConstructorArityBeforeFallback()
    {
        var provider = new Provider();
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(FixtureAttribute))!;
        var other = Constructor(owner, 0);
        var match = Constructor(owner, 4);
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(other, match));
        Assert.Same(match, PEAttributeDataFactory.Create(provider, Data())!.AttributeConstructor);
    }

    private static CustomAttributeData Data()
        => typeof(Annotated).GetCustomAttributesData().Single(attribute => attribute.AttributeType == typeof(FixtureAttribute));

    private static MethodSymbol Constructor(NamedTypeSymbol owner, int arity)
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        Set(method, "get_ContainingType", owner);
        Set(method, "get_Parameters", Enumerable.Range(0, arity).Select(index => Make<ParameterSymbol>(SymbolKind.Parameter, "p" + index)).ToImmutableArray());
        return method;
    }

    private sealed class Provider : ReflectionAttributeTypeProvider
    {
        private readonly Dictionary<Type, TypeSymbol> _types = new();
        public NamedTypeSymbol? MetadataType { get; set; }
        public Type? MissingType { get; set; }
        public NamedTypeSymbol? GetTypeByMetadataName(string name) => MetadataType;
        public TypeSymbol? ResolveType(Type type)
        {
            if (type == MissingType)
                return null;
            if (!_types.TryGetValue(type, out var symbol))
            {
                symbol = Named("Runtime", type.FullName!);
                _types.Add(type, symbol);
            }
            return symbol;
        }
    }

    public sealed class FixtureAttribute(int number, int[] values, Type type, string? optional) : Attribute
    {
        public string? Label { get; set; }
    }

    [Fixture(7, new[] { 3, 1 }, typeof(string), null, Label = "named")]
    private sealed class Annotated;
}
