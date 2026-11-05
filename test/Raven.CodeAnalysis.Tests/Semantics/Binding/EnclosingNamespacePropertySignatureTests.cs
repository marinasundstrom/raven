using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class EnclosingNamespacePropertySignatureTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    [Trait("Target", "DotNet")]
    public void DotNet_OverrideUsesNearestEnclosingType(bool reverseTrees, bool diagnosticsFirst, bool singleTree)
        => Verify(CompilationOptions.DotNet, reverseTrees, diagnosticsFirst, singleTree);

    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_OverrideUsesNearestEnclosingType(bool reverseTrees, bool diagnosticsFirst, bool singleTree)
        => Verify(CompilationOptions.DotNet.WithTargetPlatform(TargetPlatform.NeoCLR), reverseTrees, diagnosticsFirst, singleTree);

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_EnclosingPropertyTypePrecedesGlobalType()
    {
        var tree = SyntaxTree.ParseText("""
            public struct Token {}
            namespace Outer.Inner { class Token {} }
            namespace Outer.Inner.Parser {
                class Probe { val LastToken: Token? => null }
            }
            """);
        var compilation = Compilation.Create("GlobalPropertyShadow", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var declaration = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsAssignableFrom<PropertySymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.Equal("Outer.Inner.Token", property.Type.GetNullableUnderlyingType()!.ToFullyQualifiedMetadataName());
        AssertNoErrors(compilation);
    }

    private static void Verify(CompilationOptions options, bool reverseTrees, bool diagnosticsFirst, bool singleTree)
    {
        var sources = new[]
        {
            "namespace Outer { public struct Token {} }",
            """
            namespace Outer.Inner.Parser {
                internal open partial class Derived : Base {
                    override val LastToken: Token? => null
                }
            }
            """,
            """
            namespace Outer.Inner {
                internal open class Token {
                    static val Empty: Token = Token()
                }
            }
            """,
            """
            namespace Outer.Inner.Parser {
                internal abstract class Base {
                    virtual val LastToken: Token? => null
                }
            }
            """
        };
        var orderedSources = reverseTrees ? sources.Reverse().ToArray() : sources;
        var trees = singleTree
            ? [SyntaxTree.ParseText(string.Join("\n", orderedSources))]
            : orderedSources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create("EnclosingPropertySignature",
            trees, TestMetadataReferences.Default,
            options.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            AssertNoErrors(compilation);

        foreach (var declaration in trees.SelectMany(tree => tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>())
                     .Where(property => property.Identifier.ValueText == "LastToken"))
        {
            var tree = declaration.SyntaxTree;
            var property = Assert.IsAssignableFrom<PropertySymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
            var token = Assert.IsAssignableFrom<NamedTypeSymbol>(property.Type.GetNullableUnderlyingType());
            Assert.Equal("Outer.Inner.Token", token.ToFullyQualifiedMetadataName());
            Assert.Equal(TypeKind.Class, token.TypeKind);
            Assert.True(property.GetMethod!.IsVirtual);
            Assert.Same(compilation.Assembly, token.ContainingAssembly);
        }

        AssertNoErrors(compilation);
    }

    private static void AssertNoErrors(Compilation compilation)
        => Assert.DoesNotContain(compilation.SyntaxTrees.SelectMany(tree => compilation.GetSemanticModel(tree).GetDiagnostics()),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
}
