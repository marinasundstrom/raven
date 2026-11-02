using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceConstructorOwnershipTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, true, false)]
    [InlineData(true, false, false)]
    [InlineData(true, true, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, true)]
    [InlineData(true, false, true)]
    [InlineData(true, true, true)]
    public void QualifiedFieldInitializer_UsesSourceConstructor(bool reverseTrees, bool diagnosticsFirst, bool fullyQualified)
    {
        var declaration = SyntaxTree.ParseText("""
            namespace Raven.CodeAnalysis.Syntax.InternalSyntax {
                internal class SyntaxList {
                    init(values: int[]) { }
                    val Count: int => 42
                }
            }
            """);
        var receiver = fullyQualified ? "Raven.CodeAnalysis.Syntax.InternalSyntax" : "InternalSyntax";
        var consumer = SyntaxTree.ParseText($$"""
            namespace Raven.CodeAnalysis.Syntax {
                class Probe {
                    private static readonly field Empty: InternalSyntax.SyntaxList = {{receiver}}.SyntaxList([])
                    static func Read() -> int => Empty.Count
                }
            }
            """);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location));
        var compilation = Compilation.Create("SourceConstructor", reverseTrees ? [consumer, declaration] : [declaration, consumer],
            references.ToArray(), new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = consumer.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(consumer).GetSymbolInfo(invocation).Symbol);
        Assert.Equal(MethodKind.Constructor, symbol.MethodKind);
        Assert.Same(compilation.Assembly, symbol.ContainingType!.ContainingAssembly);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Raven.CodeAnalysis.Syntax.Probe")!.GetMethod("Read")!.Invoke(null, null));
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void QualifiedGenericConstructor_SelectsSourceDeclarationWithMatchingArity(bool genericFirst)
    {
        var plain = "internal class SyntaxList { }";
        var generic = "internal class SyntaxList<T> { init(value: T) { } }";
        var tree = SyntaxTree.ParseText($$"""
            namespace Raven.CodeAnalysis.Syntax {
                {{(genericFirst ? generic : plain)}}
                {{(genericFirst ? plain : generic)}}
                class Probe {
                    private static readonly field Value: SyntaxList<int> = Raven.CodeAnalysis.Syntax.SyntaxList<int>(42)
                }
            }
            """);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(SyntaxNode).Assembly.Location));
        var compilation = Compilation.Create("SourceConstructorArity", [tree], references.ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var constructor = Assert.IsAssignableFrom<IMethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        Assert.Same(compilation.Assembly, constructor.ContainingType!.ContainingAssembly);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(constructor.ContainingType.TypeArguments).SpecialType);
    }

}
