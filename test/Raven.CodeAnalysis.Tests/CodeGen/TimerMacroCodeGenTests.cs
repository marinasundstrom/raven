using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class TimerMacroCodeGenTests
{
    [Fact]
    public void TimerMacro_ExecutesBlockStatements()
    {
        var result = InvokeRun("""
            import System.Collections.Generic.*
            import Raven.Macros.*

            class Harness {
                public static func Run() -> int {
                    let values = List<int>()
                    timer! {
                        values.Add(1)
                        values.Add(42)
                    }
                    return values.Count
                }
            }
            """);

        Assert.Equal(2, result);
    }

    [Fact]
    public void TimerMacro_ReportsOrdinaryRavenBlockDiagnostics()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            func Main() -> unit {
                timer! {
                    System.Console.Write(
                }
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var bodyStart = syntaxTree.GetText().ToString().IndexOf("System.Console", StringComparison.Ordinal);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic =>
            diagnostic.Severity == DiagnosticSeverity.Error &&
            diagnostic.Location.SourceSpan.Start >= bodyStart);
    }

    [Fact]
    public void TimerMacro_WarnsInReleaseCode()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            func Main() -> unit {
                timer! {
                    System.Console.Write("")
                }
            }
            """);
        var compilation = CreateCompilation(
            syntaxTree,
            new CompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release));

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic =>
                diagnostic.GetMessage().Contains("TIMER002", StringComparison.Ordinal)));

        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(
            result.Success,
            string.Join(Environment.NewLine, result.Diagnostics.Select(static diagnostic =>
                $"{diagnostic.Location.SourceSpan}: {diagnostic.GetDescription()}")) +
            Environment.NewLine +
            compilation.GetSemanticModel(syntaxTree).GetExpandedRoot().ToFullString());

        using var loaded = TestAssemblyLoader.LoadFromStream(
            peStream,
            [.. TestMetadataReferences.Default]);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(
        SyntaxTree syntaxTree,
        CompilationOptions? options = null)
        => Compilation.Create(
                "test",
                options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros);
}
