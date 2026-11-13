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
        var ctor = Constructor(owner, provider, typeof(int), typeof(int[]), typeof(Type), typeof(string));
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
        var ctor = Constructor(owner, provider, typeof(int), typeof(int[]), typeof(Type), typeof(string));
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
    public void SelectsMatchingConstructorSignature()
    {
        var provider = new Provider();
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(FixtureAttribute))!;
        var other = Constructor(owner, provider);
        var match = Constructor(owner, provider, typeof(int), typeof(int[]), typeof(Type), typeof(string));
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(other, match));
        Assert.Same(match, PEAttributeDataFactory.Create(provider, Data())!.AttributeConstructor);
    }

    private static CustomAttributeData Data()
        => typeof(Annotated).GetCustomAttributesData().Single(attribute => attribute.AttributeType == typeof(FixtureAttribute));

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EqualArityOverloadsMatchDeclaredParametersNotBoxedArgumentTypes(bool boxed)
    {
        var provider = new Provider();
        var target = boxed ? typeof(BoxedAnnotated) : typeof(StringAnnotated);
        var attribute = target.GetCustomAttributesData().Single(item => item.AttributeType == typeof(ChoiceAttribute));
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(ChoiceAttribute))!;
        var incorrect = Constructor(owner, provider, typeof(int));
        var correct = Constructor(owner, provider, boxed ? typeof(object) : typeof(string));
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(incorrect, correct));
        var data = PEAttributeDataFactory.Create(provider, attribute)!;
        Assert.Same(correct, data.AttributeConstructor);
        Assert.Equal(boxed ? (object)7 : "text", data.ConstructorArguments[0].Value);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MissingOrUnresolvableSignatureDoesNotChooseAnotherConstructor(bool unresolved)
    {
        var provider = new Provider();
        var attribute = typeof(StringAnnotated).GetCustomAttributesData().Single(item => item.AttributeType == typeof(ChoiceAttribute));
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(ChoiceAttribute))!;
        var candidate = Constructor(owner, provider, unresolved ? typeof(string) : typeof(int));
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(candidate));
        if (unresolved)
            provider.MissingType = typeof(string);
        Assert.Null(PEAttributeDataFactory.Create(provider, attribute));
    }

    [Fact]
    public void EnumArgumentsRetainKindAndUnsignedValuesIncludingArrayAndNamedArguments()
    {
        var provider = new Provider();
        var attribute = typeof(EnumAnnotated).GetCustomAttributesData().Single(item => item.AttributeType == typeof(EnumFixtureAttribute));
        var owner = (NamedTypeSymbol)provider.ResolveType(typeof(EnumFixtureAttribute))!;
        var ctor = Constructor(owner, provider, typeof(Flags), typeof(Flags[]));
        Set(owner, "GetMembers", ImmutableArray.Create<Symbol>(ctor));
        var data = PEAttributeDataFactory.Create(provider, attribute)!;
        var scalar = data.ConstructorArguments[0];
        Assert.Equal(TypedConstantKind.Enum, scalar.Kind);
        Assert.Same(provider.ResolveType(typeof(Flags)), scalar.Type);
        Assert.Equal((ulong)Flags.High, Assert.IsType<ulong>(scalar.Value));
        Assert.All(data.ConstructorArguments[1].Values, item => Assert.Equal(TypedConstantKind.Enum, item.Kind));
        Assert.Equal((ulong)Flags.High, data.ConstructorArguments[1].Values[1].Value);
        Assert.Equal(TypedConstantKind.Enum, Assert.Single(data.NamedArguments).Value.Kind);
    }

    private static MethodSymbol Constructor(NamedTypeSymbol owner, Provider provider, params Type[] parameterTypes)
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
        Set(method, "get_ContainingType", owner);
        Set(method, "get_Parameters", parameterTypes.Select((type, index) =>
        {
            var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "p" + index);
            Set(parameter, "get_Type", provider.ResolveType(type));
            return parameter;
        }).ToImmutableArray());
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
                Set(symbol, "Equals", (Func<object?[]?, object?>)(arguments => ReferenceEquals(symbol, arguments![0])));
                _types.Add(type, symbol);
            }
            return symbol;
        }
    }

    public sealed class FixtureAttribute(int number, int[] values, Type type, string? optional) : Attribute
    {
        public string? Label { get; set; }
    }

    public sealed class ChoiceAttribute(object value) : Attribute
    {
        public ChoiceAttribute(string value) : this((object)value) { }
    }

    [Choice((object)7)]
    private sealed class BoxedAnnotated;

    [Choice("text")]
    private sealed class StringAnnotated;

    public enum Flags : ulong { Low = 1, High = 0x8000000000000000UL }

    public sealed class EnumFixtureAttribute(Flags value, Flags[] values) : Attribute
    {
        public Flags Named { get; set; }
    }

    [EnumFixture(Flags.High, new[] { Flags.Low, Flags.High }, Named = Flags.High)]
    private sealed class EnumAnnotated;

    [Fixture(7, new[] { 3, 1 }, typeof(string), null, Label = "named")]
    private sealed class Annotated;
}
