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

    [Fact]
    public void GenericReadOnlySpanParameter_InfersElementTypeFromArrayAndSpan()
    {
        const string source = """
class Test {
    static func Length<T>(values: System.ReadOnlySpan<T>) -> int {
        values.Length
    }

    static func MutableLength<T>(values: System.Span<T>) -> int {
        values.Length
    }

    static func Run() {
        val array: int[] = [1, 2, 3]
        val mutable: System.Span<int> = array
        val readOnly: System.ReadOnlySpan<int> = array
        val fromArray = Length(array)
        val fromSpan = Length(mutable)
        val fromReadOnlySpan = Length(readOnly)
        val fromString = Length("hello")
        val mutableFromArray = MutableLength(array)
        val mutableFromSpan = MutableLength(mutable)
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
    public void StringArgument_PrefersReadOnlySpanOverObjectOverload()
    {
        const string source = """
class Test {
    static func Choose(value: object) -> int { 1 }
    static func Choose(value: System.ReadOnlySpan<char>) -> int { 2 }

    static func Run() -> int {
        Choose("hello")
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
    public void ArrayArgument_PrefersReadOnlySpanOverSpanAndEnumerableOverloads()
    {
        const string source = """
class Test {
    static func Choose(value: System.Span<int>) -> int { 1 }
    static func Choose(value: System.ReadOnlySpan<int>) -> int { 2 }

    static func ChooseSequence(value: System.Collections.Generic.IEnumerable<int>) -> int { 1 }
    static func ChooseSequence(value: System.ReadOnlySpan<int>) -> int { 2 }

    static func Run() {
        val values: int[] = [1, 2, 3]
        val spanChoice = Choose(values)
        val sequenceChoice = ChooseSequence(values)
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
    public void ReadOnlySpanIndexer_IsNotSettable()
    {
        const string source = """
class Test {
    static func Run() {
        val array: int[] = [1, 2, 3]
        val values: System.ReadOnlySpan<int> = array
        values[0] = 42
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor.Id == "RAV0131");
    }
}
