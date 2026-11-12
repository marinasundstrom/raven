using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialBaseListOverrideQueryOrderTests : CompilationTestBase
{
    public static IEnumerable<object[]> QueryOrders()
    {
        foreach (var order in new[] { "012", "021", "102", "120", "201", "210" })
        {
            yield return [order, false];
            yield return [order, true];
        }
    }

    [Theory]
    [MemberData(nameof(QueryOrders))]
    [Trait("Target", "Shared")]
    public void AllTreeOrdersPreserveOverrideBinding(string order, bool withMetadataSeed)
        => VerifyQueryOrder(order, withMetadataSeed);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "Shared")]
    public void BaseHeaderUsesImportsFromItsOwnPartialFile(bool reverseTrees)
    {
        var sources = new[]
        {
            "namespace Parents\npublic open class Base { virtual func Value() -> int => 1 }",
            "namespace Parts\nimport Parents.*\nopen partial class Derived : Base { }",
            "namespace Parts\npartial class Derived { override func Value() -> int => 2 }"
        };
        var trees = sources.Select((source, index) => SyntaxTree.ParseText(source, path: $"scope{index}.rvn")).ToArray();
        var compilation = CreateCompilation(reverseTrees ? trees.Reverse() : trees);
        compilation.EnsureSourceTypeDeclarationsDeclared();
        compilation.GetSemanticModel(trees[2]).EnsureMemberSignaturesDeclared();
        using var image = new System.IO.MemoryStream();
        var emission = compilation.Emit(image);
        Assert.True(emission.Success, string.Join(System.Environment.NewLine, emission.Diagnostics));
        var declaration = trees[2].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsType<SourceMethodSymbol>(compilation.GetSemanticModel(trees[2]).GetDeclaredSymbol(declaration));
        Assert.True(method.IsOverride);
        Assert.Equal("Base", method.OverriddenMethod?.ContainingType.Name);
    }

    private void VerifyQueryOrder(string order, bool withMetadataSeed)
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
            partial class Rewriter {
                override func VisitSample(node: Sample) -> Node? => node
            }
            """
        };
        var trees = sources.Select((source, index) => SyntaxTree.ParseText("namespace Example\n" + source, path: $"part{index}.rvn")).ToArray();
        var references = TestMetadataReferences.Default;
        if (withMetadataSeed)
        {
            var seed = TestMetadataFactory.CreateFileReferenceFromSource("""
            namespace Example
            public open class Node { }
            public class Sample : Node { }
            public abstract class Visitor<T> {
                public virtual func VisitSample(node: Sample) -> T { throw System.InvalidOperationException() }
            }
            """, "OverrideSeed" + Guid.NewGuid().ToString("N"),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
            references = [.. references, seed];
        }
        var compilation = CreateCompilation(order.Select(index => trees[index - '0']),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None),
            references);
        compilation.EnsureSourceTypeDeclarationsDeclared();
        compilation.GetSemanticModel(trees[2]).EnsureMemberSignaturesDeclared();
        using var image = new System.IO.MemoryStream();
        var emission = compilation.Emit(image);
        Assert.True(emission.Success, string.Join(System.Environment.NewLine, emission.Diagnostics));
        var declaration = trees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(trees[0]).GetDeclaredSymbol(declaration));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(method.IsOverride);
        Assert.Equal("Rewriter", ((SourceMethodSymbol)method).OverriddenMethod?.ContainingType.Name);
    }
}
