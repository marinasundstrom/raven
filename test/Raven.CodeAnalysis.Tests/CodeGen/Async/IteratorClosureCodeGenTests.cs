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
    [InlineData("yield value; yield Read()", "7,8")]
    [InlineData("yield self.value; yield self.Read()", "7,8")]
    [InlineData("value += 1; yield value; yield Read()", "8,9")]
    public void InstanceIteratorUsesOriginalReceiver(string body, string expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            public class Probe {
                private var value: int = 7
                private func Read() -> int => value + 1
                func Values() -> IEnumerable<int> {
                    {{body}}
                }
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("IteratorReceiver", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var type = loaded.Assembly.GetType("Probe")!;
        var instance = Activator.CreateInstance(type);
        var values = (IEnumerable<int>)type.GetMethod("Values")!.Invoke(instance, null)!;
        Assert.Equal(expected, string.Join(",", values.ToArray()));
    }

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
