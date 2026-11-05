using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ImportedCallableTypeCollisionTests
{
    private const string Source = """
        namespace Factory {
            public static class Makers {
                static func Token(value: int) -> int => value + 1
            }
        }
        namespace Local {
            import Factory.Makers.*
            class Token {}
            class Probe {
                static func Run() -> int => Token(41)
                static func Identity(value: Token) -> Token => value
            }
        }
        """;

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "DotNet")]
    public void DotNet_ImportedFactoryRemainsCallableBesideLocalType(bool diagnosticsFirst)
        => Verify(TargetPlatform.DotNet, diagnosticsFirst);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_ImportedFactoryRemainsCallableBesideLocalType(bool diagnosticsFirst)
        => Verify(TargetPlatform.NeoCLR, diagnosticsFirst);

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_ImportedFactoryExecutesInsteadOfConstructingLocalType()
    {
        var tree = SyntaxTree.ParseText(Source);
        var compilation = CreateCompilation(tree, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Local.Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    private static void Verify(TargetPlatform target, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText(Source);
        var compilation = CreateCompilation(tree, target);
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(call).Symbol);
        Assert.Equal(MethodKind.Ordinary, method.MethodKind);
        Assert.Equal("Factory.Makers", method.ContainingType!.ToFullyQualifiedMetadataName());
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(call).Type!.SpecialType);
        var identity = compilation.GetTypeByMetadataName("Local.Probe")!.GetMembers("Identity").OfType<MethodSymbol>().Single();
        Assert.Equal("Local.Token", identity.Parameters.Single().Type.ToFullyQualifiedMetadataName());
        Assert.Equal("Local.Token", identity.ReturnType.ToFullyQualifiedMetadataName());
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation CreateCompilation(SyntaxTree tree, TargetPlatform target)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        return Compilation.Create("ImportedFactory", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
    }
}
