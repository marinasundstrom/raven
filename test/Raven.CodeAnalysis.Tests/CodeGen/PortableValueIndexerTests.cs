using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableValueIndexerTests
{
    [Fact]
    public void ValueIndexerReadsAndWritesTheReceiver()
    {
        var tree = SyntaxTree.ParseText("""
            public struct Counter {
                private var value: int
                public var self[index: int]: int {
                    get => value + index
                    set { self.value = value - index }
                }
                public func Run() -> int {
                    self[2] = 42
                    return self[0]
                }
            }
            """);
        var compilation = Compilation.Create("ValueIndexer", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsManagedReferences: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Counter")!;
        Assert.Equal(40, type.GetMethod("Run")!.Invoke(Activator.CreateInstance(type), null));
    }
}
