using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableNullableArrayLocalTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void NullableArrayLocalUsesExistingArrayCapabilityAndPreservesNull()
    {
        var tree = SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Run(value: int[], choose: bool) -> int[]? {
                    var selected: int[]? = null
                    if choose { selected = value }
                    return selected
                }
            }
            """);
        var compilation = Compilation.Create("NullableArrayLocal", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var method = (IMethodSymbol)model.GetDeclaredSymbol(syntax)!;
        var supported = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>(), allowsArrays: true);
        Assert.True(LinearMethodBody.TryLower(method, model, syntax.Body!, _ => false,
            out _, out var failure, supported), failure?.Detail);
        var unsupported = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(),
            Enum.GetValues<LinearInstructionKind>());
        Assert.False(LinearMethodBody.TryLower(method, model, syntax.Body!, _ => false,
            out _, out _, unsupported));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var run = Assembly.Load(image.ToArray()).GetType("Consumer")!.GetMethod("Run")!;
        foreach (var value in new int[]?[] { null, [42] })
            foreach (var choose in new[] { false, true })
                Assert.Same(choose ? value : null, run.Invoke(null, [value, choose]));
    }
}
