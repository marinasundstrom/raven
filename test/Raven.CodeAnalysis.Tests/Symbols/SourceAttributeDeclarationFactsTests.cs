using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceAttributeDeclarationFactsTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PrimaryConstructorSelectsOnlyMethodTargetLists(bool constructor)
    {
        var tree = SyntaxTree.ParseText("[Tag][method: Other] class C() {}");
        var declaration = tree.GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>().Single();
        Symbol owner = constructor ? Make<MethodSymbol>(SymbolKind.Method, ".ctor") : Named("Test", "C");
        var context = new SourceAttributeDeclarationContext(false, constructor, false, false, false, false);
        var lists = SourceAttributeDeclarationFacts.GetAttributeListsForDeclaration(owner, context, declaration).ToArray();
        Assert.Equal(constructor ? 1 : 2, lists.Length);
        var methodAttribute = declaration.AttributeLists.SelectMany(list => list.Attributes).Last();
        Assert.Equal(constructor, SourceAttributeDeclarationFacts.ShouldBindAttributeForOwner(owner, context, methodAttribute));
    }

    [Theory]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    public void SourcePropertiesAndEventsExcludeExplicitBackingFieldTargets(bool isEvent, bool explicitField, bool expected)
    {
        var attribute = SyntaxTree.ParseText($"[{(explicitField ? "field: " : "")}Tag] class C {{}}")
            .GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
        var context = new SourceAttributeDeclarationContext(false, false, false, !isEvent, isEvent, false);
        var owner = Make<Symbol>(isEvent ? SymbolKind.Event : SymbolKind.Property, "Member");
        Assert.Equal(expected, SourceAttributeDeclarationFacts.ShouldBindAttributeForOwner(owner, context, attribute));
    }

    [Fact]
    public void UnsupportedDeclarationReturnsNoLists()
    {
        var node = SyntaxFactory.IdentifierName("value");
        Assert.Empty(SourceAttributeDeclarationFacts.GetAttributeListsForDeclaration(Make<Symbol>(SymbolKind.Local, "value"), default, node));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AssemblyAndModuleTargetsRemainSeparate(bool module)
    {
        var root = SyntaxTree.ParseText("[assembly: A]\n[module: B]\nclass C {}").GetRoot();
        Symbol owner = module ? Make<ModuleSymbol>(SymbolKind.Module, "M") : Make<AssemblySymbol>(SymbolKind.Assembly, "A");
        var list = Assert.Single(SourceAttributeDeclarationFacts.GetAttributeListsForDeclaration(owner, default, root));
        Assert.Equal(module ? "module" : "assembly", list.Target!.Identifier.ValueText);
        Assert.True(SourceAttributeDeclarationFacts.ShouldBindAttributeForOwner(owner, default, Assert.Single(list.Attributes)));
    }
}
