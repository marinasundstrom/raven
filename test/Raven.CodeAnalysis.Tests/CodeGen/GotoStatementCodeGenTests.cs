using System;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class GotoStatementCodeGenTests
{
    [Fact]
    public void GotoStatement_JumpsToLabel()
    {
        var code = """
import System.*

func Main() {
start:
    Console.WriteLine("start")
    goto end
    Console.WriteLine("after")
end:
    Console.WriteLine("done")
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "goto", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();

        try
        {
            Console.SetOut(writer);

            var parameters = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };

            entryPoint.Invoke(null, parameters);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "start", "done" }, output);
    }

    [Fact]
    public void GotoStatement_ExitingUseScope_ReportsDiagnostic()
    {
        var code = """
import System.*

func Main() {
    if true {
        use resource = Foo("inner")
        goto exit
    }
exit:
    return
}

class Foo : IDisposable {
    public init(name: string) {}
    public Dispose() -> unit {}
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "goto-dispose", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor.Id == "RAV2503");
    }
}
