using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class NullableConditionalCodeGenTests
{
    [Theory]
    [InlineData("return if flag { 7 } else { null }")]
    [InlineData("let value: int? = if flag { 7 } else { null }; return value")]
    [InlineData("let box = Box() { Value = if flag { 7 } else { null } }; return box.Value")]
    [InlineData("let box = Box() { Value = if int.TryParse(if flag { \"7\" } else { \"invalid\" }, out var parsed) { parsed } else { null } }; return box.Value")]
    public void ValueAndNullBranchesProduceNullableRuntimeValue(string body)
    {
        var source = $$"""
            public class Box { var Value: int? { get; set; } }
            public class Probe {
                static func Run(flag: bool) -> int? { {{body}} }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NullableConditional", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var run = loaded.Assembly.GetType("Probe")!.GetMethod("Run")!;
        Assert.Equal(7, run.Invoke(null, [true]));
        Assert.Null(run.Invoke(null, [false]));
    }
}
