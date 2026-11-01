using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialGenericOverrideQueryOrderTests : CompilationTestBase
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "Shared")]
    public void DerivedOverrideBeforePartialBaseSignaturesBinds(bool reverseTrees)
    {
        var sources = new[]
        {
            """
            open class Node { }
            class Sample : Node { }
            abstract partial class Rewriter : Visitor<Node?> { }
            class DropLeaf : Rewriter {
                override func VisitSample(node: Sample) -> Node? => null
            }
            """,
            """
            abstract partial class Visitor<T> {
                virtual func VisitSample(node: Sample) -> T { throw System.InvalidOperationException() }
            }
            """,
            """
            abstract partial class Rewriter : Visitor<Node?> {
                override func VisitSample(node: Sample) -> Node? => node
            }
            """
        };
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = CreateCompilation(reverseTrees ? trees.Reverse() : trees);
        var declaration = trees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(trees[0]).GetDeclaredSymbol(declaration));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(method.IsOverride);
        Assert.Equal("Rewriter", ((SourceMethodSymbol)method).OverriddenMethod?.ContainingType.Name);
    }
}
