using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableExplicitReferenceCastTests
{
    [Theory]
    [InlineData("Node?", "Leaf")]
    [InlineData("Node?", "Node")]
    [InlineData("object?", "Leaf")]
    [InlineData("Leaf?", "Node")]
    [InlineData("IMarker?", "Leaf")]
    [InlineData("string?", "string")]
    [InlineData("object?", "string")]
    [InlineData("Node?", "Leaf?")]
    [InlineData("BaseAlias?", "Leaf")]
    public void ExplicitCast_UsesUnderlyingReferenceRelationship(string sourceType, string targetType)
    {
        var tree = SyntaxTree.ParseText($$"""
            alias BaseAlias = Node
            interface IMarker { }
            open class Node { }
            class Leaf : Node, IMarker { }
            class Probe { static func Cast(value: {{sourceType}}) -> {{targetType}} => ({{targetType}})value }
            """);
        var compilation = Create(tree);
        var cast = tree.GetRoot().DescendantNodes().OfType<CastExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var source = model.GetTypeInfo(cast.Expression).Type!;
        var target = model.GetTypeInfo(cast).Type!;
        var conversion = compilation.ClassifyConversion(source, target);
        Assert.True(conversion.Exists);
        Assert.False(conversion.IsImplicit);
        Assert.True(conversion.IsReference);
        if (sourceType == "BaseAlias?")
            Assert.True(conversion.IsAlias);
        Assert.Equal(targetType.EndsWith('?'), target.IsNullable);
        var operation = Assert.IsAssignableFrom<IConversionOperation>(model.GetOperation(cast));
        Assert.Equal(conversion, operation.Conversion);
        Assert.Equal(target, operation.Type, SymbolEqualityComparer.Default);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("Node?", "Node")]
    [InlineData("Leaf?", "object")]
    [InlineData("string?", "string")]
    public void ImplicitNullableReferenceNarrowing_RemainsRejected(string sourceType, string targetType)
    {
        var tree = SyntaxTree.ParseText($$"""
            open class Node { }
            class Leaf : Node { }
            class Probe { static func Read(value: {{sourceType}}) -> {{targetType}} => value }
            """);
        Assert.Contains(Create(tree).GetDiagnostics(), d => d.Id == "RAV1503");
    }

    [Fact]
    public void ExplicitCast_DoesNotAdmitUnrelatedClasses()
    {
        var tree = SyntaxTree.ParseText("""
            class Other { }
            class Leaf { }
            class Probe { static func Cast(value: Other?) -> Leaf => (Leaf)value }
            """);
        Assert.Contains(Create(tree).GetDiagnostics(), d => d.Id == "RAV1503");
    }

    [Fact]
    public void RuntimeCast_MatchesCSharpForNullIdentityAndInvalidType()
    {
        Assert.Null(CSharpCast(null));
        var reference = new ReferenceLeaf();
        Assert.Same(reference, CSharpCast(reference));
        Assert.Throws<InvalidCastException>(() => CSharpCast(new ReferenceNode()));
        var tree = SyntaxTree.ParseText("""
            open class Node { }
            class Leaf : Node { }
            class Probe {
                static func Down(value: Node?) -> Leaf => (Leaf)value
                static func Same(value: Node?) -> Node => (Node)value
                static func FromObject(value: object?) -> Leaf => (Leaf)value
            }
            """);
        var compilation = Create(tree);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var probe = loaded.Assembly.GetType("Probe")!;
        var leaf = Activator.CreateInstance(loaded.Assembly.GetType("Leaf")!)!;
        var node = Activator.CreateInstance(loaded.Assembly.GetType("Node")!)!;
        foreach (var name in new[] { "Down", "Same", "FromObject" })
        {
            var method = probe.GetMethod(name)!;
            Assert.Null(method.Invoke(null, new object?[] { null }));
            Assert.Same(leaf, method.Invoke(null, new[] { leaf }));
            if (name != "Same")
            {
                var failure = Assert.Throws<System.Reflection.TargetInvocationException>(() => method.Invoke(null, new[] { node }));
                Assert.IsType<InvalidCastException>(failure.InnerException);
            }
        }
    }

    private static Compilation Create(SyntaxTree tree) => Compilation.Create("NullableReferenceCast", [tree],
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private class ReferenceNode;
    private sealed class ReferenceLeaf : ReferenceNode;
    private static ReferenceLeaf? CSharpCast(ReferenceNode? value) => (ReferenceLeaf)value;
}
