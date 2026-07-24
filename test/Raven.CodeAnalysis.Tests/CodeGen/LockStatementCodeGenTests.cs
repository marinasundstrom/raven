using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class LockStatementCodeGenTests
{
    [Fact]
    public void LockStatement_HoldsAndReleasesMonitorWhenBodyThrows()
    {
        const string code = """
import System.*
import System.Threading.*

func Main() {
    val gate = Object()

    try {
        lock gate {
            Console.WriteLine(Monitor.IsEntered(gate))
            throw InvalidOperationException("boom")
        }
    } catch (Exception) {
    }

    Console.WriteLine(Monitor.IsEntered(gate))
}
""";

        Assert.Equal(["True", "False"], CompileAndRun(code));
    }

    [Fact]
    public void LockStatement_RejectsValueTypeExpression()
    {
        const string code = """
func Main() {
    lock 42 {
    }
}
""";

        var compilation = Compilation.Create("lock-value")
            .AddSyntaxTrees(SyntaxTree.ParseText(code))
            .AddReferences(TestMetadataReferences.Default);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        diagnostic.Descriptor.ShouldBe(CompilerDiagnostics.LockExpressionMustBeReferenceType);
    }

    private static string[] CompileAndRun(string code)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
                "lock-runtime",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(SyntaxTree.ParseText(code))
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
            entryPoint.Invoke(null, null);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().Split(
            Environment.NewLine,
            StringSplitOptions.RemoveEmptyEntries);
    }
}
