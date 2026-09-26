using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class GenericMethodGroupContextTests
{
    [Theory]
    [InlineData("let convert: Func<object, T> = Operations.Convert<T>\nreturn convert(value)")]
    [InlineData("return Operations.Apply<T>(value, Operations.Convert<T>)")]
    [InlineData("return Operations.Apply(value, Operations.Convert<T>)")]
    [InlineData("let echo: Func<T, T> = Operations.Echo\nreturn echo((T)value)")]
    public void EnclosingMethodTypeArgument_ExecutesForReferenceAndValueTypes(string body)
    {
        var source = $$"""
            import System.*
            class Operations {
                static func Convert<T>(value: object) -> T { return (T)value }
                static func Echo<T>(value: T) -> T { return value }
                static func Apply<T>(value: object, convert: Func<object, T>) -> T { return convert(value) }
                static func Run<T>(value: object) -> T {
                    {{body}}
                }
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_method_group_context", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var run = loaded.Assembly.GetType("Operations", true)!.GetMethod("Run")!;
        var value = new object();
        Assert.Same(value, run.MakeGenericMethod(typeof(object)).Invoke(null, new[] { value }));
        Assert.Equal(42, run.MakeGenericMethod(typeof(int)).Invoke(null, new object[] { 42 }));
    }

    [Fact]
    public void UninferredMethodTypeArgument_IsStillRejected()
    {
        const string source = """
            import System.*
            class Operations {
                static func Convert<T>(value: object) -> T { return (T)value }
                static func Run<T>() -> Func<object, T> {
                    let convert: Func<object, T> = Operations.Convert
                    return convert
                }
            }
            """;
        var compilation = Compilation.Create("uninferred_method_group", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV2203");
    }
}
