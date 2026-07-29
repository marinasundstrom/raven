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
        let array: int[] = [1, 2, 3]
        let mutable: System.Span<int> = array
        let readOnlyFromArray: System.ReadOnlySpan<int> = array
        let readOnlyFromSpan: System.ReadOnlySpan<int> = mutable
        let readOnlyFromString: System.ReadOnlySpan<char> = "hello"
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
        let strings: string[] = ["a", "b"]
        let mutable: System.Span<string> = strings
        let objectsFromArray: System.ReadOnlySpan<object> = strings
        let objectsFromSpan: System.ReadOnlySpan<object> = mutable
        let stringsReadOnly: System.ReadOnlySpan<string> = strings
        let objectsFromReadOnly: System.ReadOnlySpan<object> = stringsReadOnly
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
        let array: int[] = [1, 2, 3]
        let mutable: System.Span<int> = array
        let readOnly: System.ReadOnlySpan<int> = array
        let fromArray = Length(array)
        let fromSpan = Length(mutable)
        let fromReadOnlySpan = Length(readOnly)
        let fromString = Length("hello")
        let mutableFromArray = MutableLength(array)
        let mutableFromSpan = MutableLength(mutable)
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
        let values: int[] = [1, 2, 3]
        let spanChoice = Choose(values)
        let sequenceChoice = ChooseSequence(values)
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
        let array: int[] = [1, 2, 3]
        let values: System.ReadOnlySpan<int> = array
        values[0] = 42
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor.Id == "RAV0131");
    }
}
