using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class QueryMacroCodeGenTests
{
    [Fact]
    public void QueryMacro_FromWhereSelect_BindsRangeVariableAndEmits()
    {
        var result = InvokeRun("""
            import System.Linq.*
            import Raven.Macros.*

            class Harness {
                public static func Run() -> int {
                    let value = 100
                    let query = query! {
                        from value in [1, 2, 3, 4]
                        where value > 2
                        select value * 10
                    }

                    return value + query.Sum()
                }
            }
            """);

        Assert.Equal(170, result);
    }

    [Fact]
    public void QueryMacro_FromSelectWithoutWhere_Emits()
    {
        var result = InvokeRun("""
            import System.Linq.*
            import Raven.Macros.*

            class Harness {
                public static func Run() -> int {
                    let query = query! {
                        from value in [1, 2, 3]
                        select value + 1
                    }

                    return query.Sum()
                }
            }
            """);

        Assert.Equal(9, result);
    }

    [Fact]
    public void QueryMacro_MissingSelectClause_ReportsBodyDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            func Main() -> int => query! {
                from value in [1, 2, 3]
                where value > 1
            }
            """);

        var compilation = CreateCompilation(syntaxTree);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAVM021"));

        Assert.Contains("QUERY001: Expected a 'select' clause.", diagnostic.GetMessage(), StringComparison.Ordinal);
        Assert.Equal("where", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void QueryMacro_MalformedEmbeddedExpression_ReportsParserDiagnosticAtAuthoredLocation()
    {
        var syntaxTree = SyntaxTree.ParseText("""
            import Raven.Macros.*

            func Main() -> int => query! {
                from value in [1, 2, 3]
                where value.Equals(1, )
                select value
            }
            """);

        var compilation = CreateCompilation(syntaxTree);
        var diagnostic = Assert.Single(
            compilation.GetDiagnostics().Where(static diagnostic => diagnostic.Id == "RAV1525"));

        Assert.Same(syntaxTree, diagnostic.Location.SourceTree);
        Assert.Equal(")", syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddMacroReferences(MacroReference.CreateFromFile(
                ((PortableExecutableReference)TestMetadataReferences.RavenMacros).FilePath!));
}
