using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SequenceTypeContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SelectedSequenceIdentityDeterminesElement(bool asynchronous)
    {
        var element = Named("Item");
        var definition = Named("Selected");
        var constructed = Construct(definition, element);
        var services = new Services(asynchronous ? null : definition, asynchronous ? definition : null, Named("Char"));
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(services, constructed));
        var owner = Named("Owner");
        MetadataSymbolFixture.Set(owner, "get_AllInterfaces", ImmutableArray.Create(constructed));
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(services, owner));
    }

    [Theory]
    [InlineData("IEnumerable")]
    [InlineData("IAsyncEnumerable")]
    public void UnselectedSameNamedDefinitionsDoNotMatch(string name)
    {
        var selected = Named(name, "SelectedAssembly");
        var unrelated = Named(name, "OtherAssembly");
        var services = new Services(selected, null, Named("Char"));
        Assert.Null(SequenceTypeUtilities.TryGetElementType(services, Construct(unrelated, Named("Item"))));
    }

    [Fact]
    public void MissingDefinitionsAndWrongArityReturnNull()
    {
        var definition = Named("Sequence");
        var services = new Services(null, null, Named("Char"));
        Assert.Null(SequenceTypeUtilities.TryGetElementType(services, Construct(definition, Named("Item"))));
        services = new Services(definition, null, services.CharacterType);
        Assert.Null(SequenceTypeUtilities.TryGetElementType(services, definition));
    }

    [Fact]
    public void ArraysAndStringsDoNotRequestSequenceDefinitions()
    {
        var element = Named("Item");
        var array = MetadataSymbolFixture.Make<ArrayTypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array);
        MetadataSymbolFixture.Set(array, "get_IsNullable", false);
        MetadataSymbolFixture.Set(array, "get_ElementType", element);
        var text = Named("String");
        MetadataSymbolFixture.Set(text, "get_SpecialType", SpecialType.System_String);
        var character = Named("Char");
        var services = new Services(null, null, character);
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(services, array));
        Assert.Same(character, SequenceTypeUtilities.TryGetElementType(services, text));
        Assert.Equal(0, services.DefinitionCalls);
    }

    [Fact]
    public void NullableAndLiteralInputsAreUnwrapped()
    {
        var element = Named("Item");
        var definition = Named("Sequence");
        var constructed = Construct(definition, element);
        MetadataSymbolFixture.Set(constructed, "get_BaseType", null);
        var nullable = new NullableTypeSymbol(constructed, null, null, null, [], NullableAbiProjection.AnnotatedUnderlyingType);
        var assembly = MetadataSymbolFixture.Make<AssemblySymbol>(SymbolKind.Assembly, "Test");
        var literal = new LiteralTypeSymbol(constructed, 1, assembly);
        var services = new Services(definition, null, Named("Char"));
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(services, nullable));
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(services, literal));
    }

    private static NamedTypeSymbol Named(string name, string assembly = "Test")
    {
        var type = MetadataSymbolFixture.Named(assembly, "Example." + name);
        MetadataSymbolFixture.Set(type, "get_IsNullable", false);
        MetadataSymbolFixture.Set(type, "get_IsValueType", false);
        MetadataSymbolFixture.Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        MetadataSymbolFixture.Set(type, "get_Arity", 1);
        return type;
    }

    private static NamedTypeSymbol Construct(NamedTypeSymbol definition, TypeSymbol element)
    {
        var type = Named(definition.Name);
        MetadataSymbolFixture.Set(type, "get_OriginalDefinition", definition);
        MetadataSymbolFixture.Set(type, "get_TypeArguments", ImmutableArray.Create(element));
        return type;
    }

    private sealed class Services(NamedTypeSymbol? synchronous, NamedTypeSymbol? asyncDefinition, TypeSymbol character) : SequenceTypeServices
    {
        public TypeSymbol CharacterType => character;
        public int DefinitionCalls { get; private set; }

        public NamedTypeSymbol? GetSequenceDefinition(bool asynchronous)
        {
            DefinitionCalls++;
            return asynchronous ? asyncDefinition : synchronous;
        }
    }
}
