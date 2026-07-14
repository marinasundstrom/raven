using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class LoopStatementCodeGenTests
{
    [Fact]
    public void LoopStatement_WithBreak_EmitsAndRuns()
    {
        const string code = """
class Runner {
    public static func Run() -> int {
        var value = 0

        loop {
            value = value + 1

            if value == 3 {
                break
            }
        }

        return value
    }
}
""";

        using var loaded = EmitAssembly(code);
        var type = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(3, run.Invoke(null, Array.Empty<object>()));
    }

    private static TestAssemblyLoader.LoadedAssembly EmitAssembly(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("loop-statement", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return TestAssemblyLoader.LoadFromStream(peStream, references);
    }
}
