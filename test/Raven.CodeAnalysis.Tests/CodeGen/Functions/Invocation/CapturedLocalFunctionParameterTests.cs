using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class CapturedLocalFunctionParameterTests
{
    [Theory]
    [InlineData("func Local(text: string, count: int) -> string => \"${prefix}:${text}:${count}\"", "Local(\"value\", 42)", "captured:value:42")]
    [InlineData("func Local(text: string = \"default\") -> string => \"${prefix}:${text}\"", "Local()", "captured:default")]
    [InlineData("func Local(ref count: int) -> string { count += 2; return \"${prefix}:${count}\" }", "{ var count = 40; let result = Local(ref count); if count != 42 { return \"wrong\" }; result }", "captured:42")]
    [InlineData("func Local(out count: int) -> string { count = 42; return prefix }", "{ let result = Local(out var count); \"${result}:${count}\" }", "captured:42")]
    public void CapturedLocalFunctionUsesAuthoredParameters(string function, string invocation, string expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                static func Run() -> string {
                    let prefix = "captured"
                    {{function}}
                    return {{invocation}}
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("CapturedParameters", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
