using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class MacroDefinitionContractTests
{
    [Theory]
    [InlineData("Render", "Render", false, true)]
    [InlineData("Render", "RenderMacro", false, false)]
    [InlineData("Render", "RenderMacro", true, true)]
    [InlineData("Render", "render", true, false)]
    [InlineData("Render", "rendermacro", true, false)]
    [InlineData("RenderMacro", "RenderMacroMacro", true, true)]
    [InlineData("RenderMacro", "Render", true, false)]
    public void DeclaredNameMatchingIsOrdinalAndSuffixIsOptIn(string declaredName, string requestedName, bool allowSuffix, bool expected)
        => Assert.Equal(expected, MacroNaming.MatchesDeclaredName(declaredName, requestedName, allowSuffix));

    [Fact]
    public void DefinitionUsesCanonicalBaseAndPreservesDeclarationIdentity()
    {
        var services = Make<SourceDeclarationServices>(SymbolKind.Assembly, "Services");
        var objectType = Named("Test", "System.Object");
        Set(services, "GetSpecialType", (Func<object?[]?, object?>)(args =>
        {
            Assert.Equal(SpecialType.System_Object, args![0]);
            return objectType;
        }));
        var ns = Make<IContainingNamespace>(SymbolKind.Namespace, "Macros");
        var syntax = SyntaxTree.ParseText("class Declaration {}").GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var reference = syntax.GetReference();
        var location = syntax.GetLocation();
        var definition = new SynthesizedMacroDefinitionTypeSymbol(services, "Render", ns,
            [location], [reference], Accessibility.Internal);
        Assert.Equal("RenderMacro", definition.Name);
        Assert.Equal("RenderMacro", definition.MetadataName);
        Assert.Same(objectType, definition.BaseType);
        Assert.Same(ns, definition.ContainingSymbol);
        Assert.Same(ns, definition.ContainingNamespace);
        Assert.Null(definition.ContainingType);
        Assert.Same(location, Assert.Single(definition.Locations));
        Assert.Same(reference, Assert.Single(definition.DeclaringSyntaxReferences));
        Assert.True(definition.IsImplicitlyDeclared);
        Assert.True(definition.IsClosed);
        Assert.False(definition.IsStatic);
        Assert.Equal(Accessibility.Internal, definition.DeclaredAccessibility);
        var parameter = Parameter("T", 0);
        definition.SetTypeParameters([parameter]);
        Assert.Same(parameter, Assert.Single(definition.TypeParameters));
        Assert.Equal("RenderMacro`1", definition.MetadataName);
    }
    internal interface IContainingNamespace : NamespaceSymbol, SymbolMemberContainer { }
}
