using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceTypeDeclarationServiceTests
{
    [Fact]
    public void SpecialTypeClassificationIsOwnedByDeclarationServices()
    {
        var services = Services();
        NamedTypeSymbol? requested = null;
        Set(services, "GetSourceTypeSpecialType", (Func<object?[]?, object?>)(args =>
        {
            requested = (NamedTypeSymbol)args![0]!;
            return SpecialType.System_Array;
        }));
        var type = new ProviderType(services, []);
        Assert.Equal(SpecialType.System_Array, type.SpecialType);
        Assert.Same(type, requested);
        Assert.Null(type.BaseType);
    }

    [Fact]
    public void PrimaryConstructorCompletionIsRequestedOnce()
    {
        var services = Services();
        var calls = 0;
        Set(services, "EnsureSourceDeclarationsComplete", (Func<object?[]?, object?>)(_ => { calls++; return null; }));
        var declaration = SyntaxTree.ParseText("class C(value: int) {}").GetRoot()
            .DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = new ProviderType(services, [declaration.GetReference()]);
        Assert.Empty(type.GetMembers());
        Assert.Empty(type.GetMembers());
        Assert.Equal(1, calls);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ArrayProviderUsesDeclarationCompletionAndShape(bool complete)
    {
        var services = Services();
        Set(services, "get_SourceDeclarationsComplete", complete);
        var type = new ProviderType(services, []);
        var array = Make<ArrayTypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array);
        var shape = Named("Test", "Shape");
        var contract = Named("Test", "Contract");
        Set(shape, "get_AllInterfaces", ImmutableArray.Create(contract));
        Set(services, "GetArrayShape", (Func<object?[]?, object?>)(args =>
        {
            Assert.Same(type, args![0]);
            Assert.Same(array, args[1]);
            return shape;
        }));
        var provider = (IArrayTypeProvider)type;
        Assert.Equal(complete, provider.AreInterfacesComplete);
        Assert.Same(contract, Assert.Single(provider.GetAdditionalInterfaces(array)));
    }

    private static SourceTypeDeclarationServices Services()
        => Make<SourceTypeDeclarationServices>(SymbolKind.Namespace, "Services");

    private sealed class ProviderType(SourceTypeDeclarationServices services, SyntaxReference[] references)
        : SourceNamedTypeSymbol("C", null, TypeKind.Class, Make<Symbol>(SymbolKind.Namespace, "Owner"),
            null, null, [], references, addAsMember: false), SourceDeclarationServiceProvider
    {
        public SourceDeclarationServices DeclarationServices => services;
    }
}
