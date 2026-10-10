using System.Reflection;
using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableArrayComprehensionTests
{
    [Theory]
    [InlineData("0..<4", "i + 1", 4, 1, 4)]
    [InlineData("2..4", "i * 2", 3, 4, 8)]
    [InlineData("4..<4", "i", 0, 0, 0)]
    [InlineData("4..2", "i", 0, 0, 0)]
    [InlineData("2147483647..2147483647", "i", 1, int.MaxValue, int.MaxValue)]
    public void ConstantRangeArraysPreserveDotNetResults(string range, string selector, int length, int first, int last)
    {
        var (compilation, syntax, model) = Create($"[for i in {range} => {selector}]");
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model,
            syntax.Body!, _ => false, out _, out var failure, Capabilities()), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var values = (int[])Assembly.Load(image.ToArray()).GetType("Arrays")!.GetMethod("Run")!.Invoke(null, [3])!;
        Assert.Equal(length, values.Length);
        if (length > 0)
        {
            Assert.Equal(first, values[0]);
            Assert.Equal(last, values[^1]);
        }
    }

    [Theory]
    [InlineData("[for i in 0..<n => i]")]
    [InlineData("[for i in 0..<4 if i > 1 => i]")]
    [InlineData("[for i in 0..2147483647 => i]")]
    [InlineData("[1, ...[|2, 3|]]")]
    public void UnsupportedShapesRemainDiagnosed(string expression)
    {
        var (_, syntax, model) = Create(expression);
        Assert.False(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, model,
            syntax.Body!, _ => false, out _, out var failure, Capabilities()));
        Assert.NotNull(failure);
    }

    [Fact]
    public void FilteredInclusiveMaximumTerminatesOnDotNet()
    {
        var (compilation, _, _) = Create("[for i in 2147483647..2147483647 if i < 0 => i]");
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        var values = (int[])Assembly.Load(image.ToArray()).GetType("Arrays")!.GetMethod("Run")!.Invoke(null, [0])!;
        Assert.Empty(values);
    }

    [Fact]
    public void DictionaryInclusiveMaximumTerminatesOnDotNet()
    {
        var tree = SyntaxTree.ParseText("""
            public static class Dictionaries {
                public static func Run() -> int {
                    let values: System.Collections.Generic.Dictionary<int, int> =
                        [for i in 2147483647..2147483647 => i: i]
                    return values.Count
                }
            }
            """);
        var compilation = Compilation.Create("DictionaryMaximum" + Guid.NewGuid().ToString("N"),
            [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Assert.Equal(1, Assembly.Load(image.ToArray()).GetType("Dictionaries")!.GetMethod("Run")!.Invoke(null, null));
    }

    private static EmissionCapabilities Capabilities() => new(
        Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
        allowsArrays: true, allowsRangeEnumeration: true);

    private static (Compilation, MethodDeclarationSyntax, SemanticModel) Create(string expression)
    {
        var tree = SyntaxTree.ParseText($$"""
            public static class Arrays {
                public static func Run(n: int) -> int[] {
                    return {{expression}}
                }
            }
            """);
        var compilation = Compilation.Create("PortableArray" + Guid.NewGuid().ToString("N"),
            [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        return (compilation, tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(),
            compilation.GetSemanticModel(tree));
    }
}
