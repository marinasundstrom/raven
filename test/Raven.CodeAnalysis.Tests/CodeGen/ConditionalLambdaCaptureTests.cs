using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalLambdaCaptureTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    public void ConditionalReturnRetainsOuterLocal(bool generic, bool parenthesized, bool nullable)
    {
        var signature = generic ? "Apply<T>(unused: T" : "Apply(unused: int";
        var body = "if value == 0 { " + (nullable ? "null" : "-1") + " } else { captured }";
        var resultType = nullable ? "int?" : "int";
        var fallback = nullable ? " ?? -1" : "";
        if (parenthesized)
            body = "(" + body + ")";
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                private static func {{signature}}, mapper: System.Func<int, {{resultType}}>) -> {{resultType}} {
                    return mapper(1)
                }
                static func Run() -> int {
                    let captured = 12
                    return Apply(0, func(value) => {{body}}){{fallback}}
                }
            }
            """);
        var compilation = Compilation.Create("ConditionalCapture" + Guid.NewGuid().ToString("N"), [tree],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(12, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
