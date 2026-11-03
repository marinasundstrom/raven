using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ClassConstraintReferenceTypeTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void ClassConstrainedNullReturn_ExecutesAsNullReference()
    {
        var tree = SyntaxTree.ParseText("""
            open class Node {}
            class Probe { static func Empty<T>() -> T where T: Node => null! }
            """);
        var compilation = Compilation.Create("NullClassConstraint", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var node = loaded.Assembly.GetType("Node")!;
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Empty")!.MakeGenericMethod(node);
        Assert.Null(method.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "Shared")]
    public void SourceClassConstraint_ProvesReferenceType(bool chained)
    {
        var tree = SyntaxTree.ParseText($$"""
            open class Node {}
            class Probe {
                static func Empty<{{(chained ? "T, U" : "T")}}>() -> T
                    where T: {{(chained ? "U" : "Node")}} {{(chained ? "where U: Node" : "")}} => null!
            }
            """);
        var compilation = Compilation.Create("ReferenceConstraint", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var declaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.All(method.TypeParameters, parameter => Assert.True(parameter.IsReferenceType));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("ClassBound`1", true)]
    [InlineData("Chained`2", true)]
    [InlineData("InterfaceBound`1", false)]
    [InlineData("ValueBound`1", false)]
    [InlineData("EnumBound`1", false)]
    [Trait("Target", "DotNet")]
    public void MetadataConstraints_ReportKnownReferenceTypes(string name, bool expected)
    {
        var reference = TestMetadataFactory.CreateFileReferenceFromSource("""
            public open class Node {}
            public interface IMarker {}
            public class ClassBound<T> where T : Node {}
            public class Chained<T, U> where T : U where U : Node {}
            public class InterfaceBound<T> where T : IMarker {}
            public class ValueBound<T> where T : struct {}
            public class EnumBound<T> where T : System.Enum {}
            """, "ConstraintMetadata");
        var compilation = Compilation.Create("MetadataConstraints", [], TestMetadataReferences.Default.Append(reference).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var type = compilation.GetTypeByMetadataName(name)!;
        Assert.Equal(expected, type.TypeParameters[0].IsReferenceType);
    }
}
