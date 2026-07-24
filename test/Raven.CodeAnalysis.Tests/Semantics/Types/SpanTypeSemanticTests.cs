using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SpanTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void BuiltInSpanConversions_BindWithoutErrors()
    {
        const string source = """
class Test {
    static func Run() {
        val array: int[] = [1, 2, 3]
        val mutable: System.Span<int> = array
        val readOnlyFromArray: System.ReadOnlySpan<int> = array
        val readOnlyFromSpan: System.ReadOnlySpan<int> = mutable
        val readOnlyFromString: System.ReadOnlySpan<char> = "hello"
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(
            !diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));
    }

    [Fact]
    public void CovariantReadOnlySpanConversions_BindWithoutErrors()
    {
        const string source = """
class Test {
    static func Run() {
        val strings: string[] = ["a", "b"]
        val mutable: System.Span<string> = strings
        val objectsFromArray: System.ReadOnlySpan<object> = strings
        val objectsFromSpan: System.ReadOnlySpan<object> = mutable
        val stringsReadOnly: System.ReadOnlySpan<string> = strings
        val objectsFromReadOnly: System.ReadOnlySpan<object> = stringsReadOnly
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(
            !diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error),
            string.Join(Environment.NewLine, diagnostics));
    }
}
