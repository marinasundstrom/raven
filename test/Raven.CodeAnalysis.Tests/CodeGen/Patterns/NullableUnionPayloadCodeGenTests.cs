using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class NullableUnionPayloadCodeGenTests
{
    [Theory]
    [InlineData(null, "null")]
    [InlineData("payload", "value")]
    [InlineData(42, "value")]
    public void InferredCaseVariableAcceptsNullablePayload(object? payload, string expected)
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Result.*
            public static class Consumer {
                public static func Narrow(input: object?) -> string {
                    let result: Result<object?, string> = Ok(input)
                    return match result {
                        Ok(string text) => text
                        Ok(_) => "other"
                        Error(_) => "error"
                    }
                }
                public static func Read(input: object?) -> string {
                    let result: Result<object?, string> = Ok(input)
                    return match result {
                        Ok(let value) => if value is null { "null" } else { "value" }
                        Error(_) => "error"
                    }
                }
            }
            """);
        var references = TestMetadataReferences.Default.Append(
            MetadataReference.CreateFromFile(Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"))).ToArray();
        var compilation = Compilation.Create("NullableUnionPayload", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var method = loaded.Assembly.GetType("Consumer")!.GetMethod("Read")!;
        Assert.Equal(expected, method.Invoke(null, [payload]));
        Assert.Equal(payload as string ?? "other",
            loaded.Assembly.GetType("Consumer")!.GetMethod("Narrow")!.Invoke(null, [payload]));
    }
}
