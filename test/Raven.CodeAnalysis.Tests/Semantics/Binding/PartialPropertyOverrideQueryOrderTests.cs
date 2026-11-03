using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class PartialPropertyOverrideQueryOrderTests
{
    [Theory]
    [InlineData("abstract class Container { abstract var Value: int { get; set; } }", true, false, false)]
    [InlineData("open class Container { virtual var Value: int { get; set; } }", false, false, false)]
    [InlineData("class Container { override var Value: int { get; set; } }", false, true, false)]
    [InlineData("class Container { final override var Value: int { get; set; } }", false, true, true)]
    [InlineData("interface Container { var Value: int { get; set; } }", true, false, false)]
    public void PreliminaryAccessors_PreserveInheritanceModifiers(
        string source, bool isAbstract, bool isOverride, bool isFinal)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("PropertySignatureModifiers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        compilation.EnsureSourceTypeDeclarationsDeclared();
        var declaration = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        MemberSignatureDeclarationPass.DeclarePropertySignature(compilation.GetSemanticModel(tree), declaration);
        Assert.True(compilation.TryGetPropertySymbol(declaration, out var property));
        foreach (var accessor in new[] { property.GetMethod, property.SetMethod })
        {
            var method = Assert.IsType<SourceMethodSymbol>(accessor);
            Assert.True(method.IsSignatureSkeleton);
            Assert.True(method.IsVirtual);
            Assert.Equal(isAbstract, method.IsAbstract);
            Assert.Equal(isOverride, method.IsOverride);
            Assert.Equal(isFinal, method.IsFinal);
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void DerivedPropertyOverride_AdmitsLaterPartialBaseMembers(bool reverseTrees, bool diagnosticsFirst)
    {
        var sources = new[] {
            """
            abstract partial class Visitor<T> { }
            abstract partial class Rewriter : Visitor<int> { }
            class Leaf : Rewriter { override val Value: int => 2 }
            """,
            "abstract partial class Visitor<T> { abstract val Value: T { get; } }",
            "abstract partial class Rewriter : Visitor<int> { override val Value: int => 1 }"
        };
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create("PartialPropertyOverride", reverseTrees ? trees.Reverse().ToArray() : trees,
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var declaration = trees[0].GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsAssignableFrom<PropertySymbol>(compilation.GetSemanticModel(trees[0]).GetDeclaredSymbol(declaration));
        Assert.True(property.GetMethod!.IsOverride);
        Assert.True(property.GetMethod.IsVirtual);
        Assert.Equal("Rewriter", ((SourceMethodSymbol)property.GetMethod).OverriddenMethod?.ContainingType.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
