using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class CompileMacroCodeGenTests
{
    [Fact]
    public void CompileMacro_SyntaxHole_ProducesStronglyTypedDelegate()
    {
        var result = InvokeRun("""
            import System.*
            import Raven.Macros.*

            class Harness {
                public static func Run() -> int {
                    let increment = compile<System.Func<int, int>>! {
                        value => #(Raven.CodeAnalysis.Syntax.SyntaxFactory.IdentifierName("value")) + 1
                    }

                    return increment(41)
                }
            }
            """);

        Assert.Equal(42, result);
    }

    [Fact]
    public void CompileMacro_WithoutDelegateType_ReportsDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            func Main() -> unit {
                let increment = compile! {
                    value => value + 1
                }
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains(
            "COMPILE001: The compile macro requires exactly one delegate type argument",
            diagnostic.GetMessage(),
            StringComparison.Ordinal);
    }

    [Fact]
    public void CompileMacro_WithMultipleDelegateTypes_ReportsDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import System.*
            import Raven.Macros.*

            func Main() -> unit {
                let increment = compile<Func<int>, Func<int>>! {
                    () => 1
                }
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains(
            "COMPILE001: The compile macro requires exactly one delegate type argument",
            diagnostic.GetMessage(),
            StringComparison.Ordinal);
    }

    [Fact]
    public void CompileMacro_SemanticallyInvalidExpression_ReportsRuntimeDiagnostics()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import System.*
            import Raven.Macros.*

            class Harness {
                public static func Run() -> System.Func<int, int> {
                    return compile<System.Func<int, int>>! {
                        value => value.MissingMember()
                    }
                }
            }
            """);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(
            peStream,
            [.. TestMetadataReferences.Default, CodeAnalysisReference]);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        var invocationException = Assert.Throws<TargetInvocationException>(
            () => method!.Invoke(null, null));
        var compilationException = Assert.IsType<RavenCompilationException>(
            invocationException.InnerException);
        Assert.Contains(
            compilationException.Diagnostics,
            static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(
            result.Success,
            string.Join(
                Environment.NewLine,
                result.Diagnostics.Select(diagnostic =>
                    $"{diagnostic.Location.SourceSpan}: {diagnostic.GetDescription()}")) +
            Environment.NewLine +
            compilation.GetSemanticModel(syntaxTree).GetExpandedRoot().ToFullString());

        using var loaded = TestAssemblyLoader.LoadFromStream(
            peStream,
            [.. TestMetadataReferences.Default, CodeAnalysisReference]);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddReferences(CodeAnalysisReference);

    private static MetadataReference CodeAnalysisReference
        => MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location);
}
