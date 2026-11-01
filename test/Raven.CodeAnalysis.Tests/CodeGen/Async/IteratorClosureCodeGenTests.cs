using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen.Async;

[Trait("Target", "DotNet")]
public class IteratorClosureCodeGenTests
{
    [Theory]
    [InlineData("for item in [1, 2] { let read = () => item; yield read() }", "1,2")]
    [InlineData("var value = 1; let read = () => value; yield read(); value = 2; yield read()", "1,2")]
    [InlineData("let read = () => start + 1; yield read(); yield start", "11,10")]
    [InlineData("var value = 1; let update = () => { value += 1; return value }; yield update(); yield value; yield update()", "2,2,3")]
    public void IteratorAndCallbacksShareCaptureStorage(string body, string expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.*
            import System.Collections.Generic.*
            public class Probe {
                static func Values(start: int) -> IEnumerable<int> {
                    {{body}}
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("IteratorCapture", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var values = (IEnumerable<int>)loaded.Assembly.GetType("Probe")!.GetMethod("Values")!.Invoke(null, [10])!;
        Assert.Equal(expected, string.Join(",", values.ToArray()));
    }
}
