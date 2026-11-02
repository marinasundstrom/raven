using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NullLiteralSuppressionCodeGenTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void SuppressedNullReference_RemainsNullAtRuntime()
    {
        const string source = """
            class Probe {
                static func Make() -> string => null!
                static func IsNull(value: string) -> bool => value is null
                static func Run() -> bool {
                    var value: string = null!
                    value = null!
                    return IsNull(value) && IsNull(null!)
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("SuppressedNullReference", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var probe = loaded.Assembly.GetType("Probe", true)!;
        Assert.Null(probe.GetMethod("Make")!.Invoke(null, null));
        Assert.Equal(true, probe.GetMethod("Run")!.Invoke(null, null));
    }
}
