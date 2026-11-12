using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class NestedHelperEmissionTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void DeepNestedHelperCanBeCalledFromContainingStaticType(bool inherits, bool generic)
    {
        var source = """
            open class Visitor {
                virtual func Visit() { }
            }
            class OuterGENERIC {
                static func Run() -> int { Analysis.Analyze() }
                private static class Analysis {
                    static func Analyze() -> int { Collector.Collect() }
                    private class Collector BASE {
                        static func Collect() -> int { 42 }
                    }
                }
            }
            """.Replace("BASE", inherits ? ": Visitor" : "")
            .Replace("GENERIC", generic ? "<T>" : "");
        var compilation = Compilation.Create("NestedHelpers" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        var outer = assembly.GetType(generic ? "Outer`1" : "Outer")!;
        if (generic)
            outer = outer.MakeGenericType(typeof(int));
        Assert.Equal(42, outer.GetMethod("Run")!.Invoke(null, null));
    }
}
