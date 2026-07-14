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

    [Fact]
    public void LabeledBreak_TargetsOuterLoop()
    {
        const string code = """
class Runner {
    public static func Run() -> int {
        var value = 0

outer:  loop {
            value = value + 1

            loop {
                break outer
            }

            value = 100
        }

        return value
    }
}
""";

        using var loaded = EmitAssembly(code);
        var type = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(1, run.Invoke(null, Array.Empty<object>()));
    }

    [Fact]
    public void LabeledContinue_TargetsOuterLoop()
    {
        const string code = """
class Runner {
    public static func Run() -> int {
        var value = 0

outer:  loop {
            value = value + 1

            if value == 3 {
                break
            }

            loop {
                continue outer
            }

            value = 100
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

    [Fact]
    public void LabeledBreak_TargetsOuterForLoop()
    {
        const string code = """
class Runner {
    public static func Run() -> int {
        var value = 0

outer:  for i in 0..<5 {
            value = value + 1

            loop {
                break outer
            }

            value = 100
        }

        return value
    }
}
""";

        using var loaded = EmitAssembly(code);
        var type = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(1, run.Invoke(null, Array.Empty<object>()));
    }

    [Fact]
    public void LabeledContinue_TargetsOuterForLoop()
    {
        const string code = """
class Runner {
    public static func Run() -> int {
        var value = 0

outer:  for i in 0..<3 {
            value = value + 1

            loop {
                continue outer
            }

            value = 100
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
