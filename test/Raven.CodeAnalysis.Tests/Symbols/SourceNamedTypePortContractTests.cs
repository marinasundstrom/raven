using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceNamedTypePortContractTests
{
    [Fact]
    public void GenericConstructionRetainsDefinitionAndValidatesArity()
    {
        var type = Create();
        Assert.Same(type, type.Construct());
        var parameter = Parameter("T", 0);
        type.SetTypeParameters([parameter]);
        Assert.Equal("C`1", type.MetadataName);
        Assert.Same(parameter, Assert.Single(type.TypeParameters));
        Assert.Same(parameter, Assert.Single(type.TypeArguments));
        var argument = Named("Test", "Argument");
        var constructed = Assert.IsAssignableFrom<NamedTypeSymbol>(type.Construct(argument));
        Assert.Same(type, constructed.ConstructedFrom);
        Assert.Same(argument, Assert.Single(constructed.TypeArguments));
        Assert.Throws<ArgumentException>(() => type.Construct());
    }

    [Fact]
    public void MembersPreserveIdentityOrderAndRemoval()
    {
        var type = Create();
        var first = Make<Symbol>(SymbolKind.Label, "First");
        var second = Make<Symbol>(SymbolKind.Label, "Second");
        type.AddMember(first);
        type.AddMember(first);
        type.AddMember(second);
        Assert.Equal(new[] { first, second }, type.GetMembers());
        Assert.Same(second, Assert.Single(type.GetMembers("Second")));
        type.RemoveMember(first);
        Assert.Same(second, Assert.Single(type.GetDeclaredMembersWithoutEnsuring()));
        Assert.False(type.IsMemberDefined("First", out var absent));
        Assert.Null(absent);
        Assert.True(type.IsMemberDefined("Second", out var found));
        Assert.Same(second, found);
    }

    [Fact]
    public void InterfaceClosureDeduplicatesDiamondsAndInvalidatesOnChange()
    {
        var type = Create();
        var root = Interface("Root");
        var left = Interface("Left", root);
        var right = Interface("Right", root);
        type.SetInterfaces([left, right]);
        Assert.Equal(new[] { left, root, right }, type.AllInterfaces);
        Assert.Equal(type.AllInterfaces, type.AllInterfaces);
        type.SetInterfaces([right]);
        Assert.Equal(new[] { right, root }, type.AllInterfaces);
        type.SetInterfaces([]);
        Assert.Empty(type.AllInterfaces);
    }

    [Fact]
    public void CompletedMethodReplacesSignaturePlaceholder()
    {
        var type = Create();
        var skeleton = Method<SourceMethodDeclarationIdentity>(type, "F");
        var completed = Method<SourceMethodDeclarationIdentity>(type, "F");
        Set(skeleton, "get_IsSignatureSkeleton", true);
        Set(completed, "get_IsSignatureSkeleton", false);
        type.AddMember(skeleton);
        type.AddMember(completed);
        Assert.Same(completed, Assert.Single(type.GetMembers()));
    }

    [Fact]
    public void GeneratedEntryPointAndSourceMainRemainDistinct()
    {
        var type = Create();
        var source = Method<MethodSymbol>(type, "Main");
        var generated = Method<SynthesizedEntryPointMethod>(type, "Main");
        var declaration = SyntaxTree.ParseText("func Main() {}").GetRoot()
            .DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        Set(source, "get_DeclaringSyntaxReferences", ImmutableArray.Create(declaration.GetReference()));
        type.AddMember(source);
        type.AddMember(generated);
        Assert.Equal(new Symbol[] { source, generated }, type.GetMembers("Main"));
    }

    private static SourceNamedTypeSymbol Create()
        => new("C", null, TypeKind.Class, Make<Symbol>(SymbolKind.Namespace, "Owner"),
            null, null, [], [], addAsMember: false);

    private static NamedTypeSymbol Interface(string name, params NamedTypeSymbol[] inherited)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Interface);
        Set(type, "get_Interfaces", inherited.ToImmutableArray());
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        Set(type, "get_ConstructedFrom", type);
        return type;
    }

    private static T Method<T>(NamedTypeSymbol owner, string name) where T : class
    {
        var method = Make<T>(SymbolKind.Method, name);
        Set(method, "get_ContainingType", owner);
        Set(method, "get_MethodKind", MethodKind.Ordinary);
        Set(method, "get_IsStatic", true);
        Set(method, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        Set(method, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
        return method;
    }
}
