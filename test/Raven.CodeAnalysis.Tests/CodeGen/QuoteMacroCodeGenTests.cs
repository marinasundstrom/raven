using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class QuoteMacroCodeGenTests
{
    [Fact]
    public void QuoteMacro_Expression_ProducesSyntaxAtRuntime()
    {
        var result = InvokeRun("""
            import System.*

            class Harness {
                public static func Run() -> int {
                    let syntax = #quote {
                        left + right
                    }

                    return syntax.ToString().Length
                }
            }
            """);

        Assert.Equal(12, result);
    }

    [Fact]
    public void QuoteMacro_Expression_PreservesTrivia()
    {
        var result = InvokeRun("""
            import System.*

            class Harness {
                public static func Run() -> bool {
                    let syntax = #quote {
                        /* retained */ left + right
                    }

                    return syntax.ToFullString().Contains("/* retained */")
                }
            }
            """);

        Assert.Equal(true, result);
    }

    [Fact]
    public void QuoteMacro_MalformedExpression_ReportsNativeParserDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> unit {
                let syntax = #quote {
                    value.Equals(1, )
                }
            }
            """);

        var compilation = CreateCompilation(syntaxTree, includeCodeAnalysisReference: true);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
        Assert.Equal(")", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void QuoteMacro_TrailingExpressionInput_ReportsNativeParserDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> unit {
                let syntax = #quote {
                    left right
                }
            }
            """);

        var compilation = CreateCompilation(syntaxTree, includeCodeAnalysisReference: true);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Equal("right", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void QuoteMacro_IncompleteExpression_ReportsQuoteDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> unit {
                let syntax = #quote {
                }
            }
            """);

        var compilation = CreateCompilation(syntaxTree, includeCodeAnalysisReference: true);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains(
            "QUOTE001: Quoted expression is incomplete.",
            diagnostic.GetMessage(),
            StringComparison.Ordinal);
        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
    }

    [Fact]
    public void QuoteMacro_WithoutCodeAnalysisReference_ReportsDependencyDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            func Main() -> unit {
                let syntax = #quote { 42 }
            }
            """);

        var compilation = CreateCompilation(syntaxTree, includeCodeAnalysisReference: false);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains(
            "QUOTE003: Expression quotes require a runtime reference to Raven.CodeAnalysis.",
            diagnostic.GetMessage(),
            StringComparison.Ordinal);
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree, includeCodeAnalysisReference: true);

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

    private static Compilation CreateCompilation(
        SyntaxTree syntaxTree,
        bool includeCodeAnalysisReference)
    {
        var compilation = Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        return includeCodeAnalysisReference
            ? compilation.AddReferences(CodeAnalysisReference)
            : compilation;
    }

    private static MetadataReference CodeAnalysisReference
        => MetadataReference.CreateFromFile(typeof(Compilation).Assembly.Location);
}
