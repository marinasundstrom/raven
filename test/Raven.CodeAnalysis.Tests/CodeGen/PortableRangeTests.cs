using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableRangeTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SignedRangesRequireCapabilityAndPreserveDotNetResults(bool enabled)
    {
        const string source = """
            public static class Ranges {
                public static func Run(zero: int) -> int {
                    var total = 0
                    for value in 1..5 by 2 { total += value }
                    for value in 5..1 by -2 { total += value }
                    for value in 0..<3 { total += value }
                    for value in 3..<0 by -1 { total += value }
                    for value in 1..3 by zero { return 1 }
                    for value in 1..3 by -1 { return 2 }
                    outer: for value in 1..3 {
                        for inner in 1..3 {
                            if inner == 2 { continue outer }
                            total += 1
                        }
                    }
                    for value in 1..10 {
                        if value == 1 { continue }
                        if value == 3 { break }
                        total += value
                    }
                    let start: long = 4294967296
                    let end: long = 4294967300
                    let step: long = 2
                    for value in start..<end by step { total += 5 }
                    return total
                }
            }
            """;
        var compilation = Compilation.Create("PortableRanges" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRangeEnumeration: enabled);
        var success = LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities);
        Assert.True(success == enabled, failure?.Detail);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(stream.ToArray()).GetType("Ranges")!.GetMethod("Run")!.Invoke(null, [0]));
    }
}
