using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableFriendAdmissionTests
{
    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void InternalExternalLocalsAndConstructorsRequireExplicitAdmission(string kind)
    {
        var provider = TestMetadataFactory.CreateFileReferenceFromSource("""
            import System.Runtime.CompilerServices.*
            [assembly: InternalsVisibleTo("PortableFriend")]
            internal KIND Hidden {
                internal init() {}
                internal func Answer() -> int => 42
            }
            """.Replace("KIND", kind), "PortableFriendProvider" + kind);
        var tree = SyntaxTree.ParseText("""
            public static class Entry {
                public static func Run() -> int {
                    let value = Hidden()
                    return value.Answer()
                }
            }
            """);
        var compilation = Compilation.Create("PortableFriend", [tree], TestMetadataReferences.Default.Append(provider).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        foreach (var admit in new[] { false, true })
        {
            var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
                Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
                allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsExternalReferenceSignatures: true,
                allowsExternalValueSignatures: true, allowsExternalInstanceCalls: true, allowsExternalValueInstanceCalls: true,
                allowsExternalConstructors: true, allowsManagedReferences: true, allowsInternalExternalSymbols: admit);
            Assert.True(SourceCallablePlan.TryCreate(method, out var plan, capabilities));
            var lowered = plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities);
            Assert.True(lowered == admit, failure?.Detail);
        }
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Entry")!.GetMethod("Run")!.Invoke(null, null));
    }
}
