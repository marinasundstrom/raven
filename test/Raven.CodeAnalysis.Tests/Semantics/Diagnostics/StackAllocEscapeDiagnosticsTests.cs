using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StackAllocEscapeDiagnosticsTests : DiagnosticTestBase
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("return values[1..3]")]
    [InlineData("let slice = values[1..3]\nreturn slice")]
    [InlineData("return values.Slice(1, 2)")]
    public void StackAllocSlice_CannotEscapeThroughRangeIndexing(string body)
    {
        var code = $$"""
        func Create() -> System.Span<int> {
            let values: System.Span<int> = stackalloc int[4]
            {{body}}
        }
        """;
        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id).WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void SpanParameterSlice_CanBeReturned()
    {
        const string code = """
        func Slice(values: System.ReadOnlySpan<int>) -> System.ReadOnlySpan<int> {
            return values[1..3]
        }
        """;
        CreateVerifier(code).Verify();
    }

    [Fact]
    public void StackAlloc_CannotBeReturnedDirectly()
    {
        const string code = """
        func Create() -> System.Span<int> {
            return stackalloc int[1]
        }
        """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void StackAllocBackedLocal_CannotBeReturned()
    {
        const string code = """
        func Create() -> System.Span<int> {
            let values: System.Span<int> = stackalloc int[1]
            return values
        }
        """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void StackAllocBackedAlias_CannotBeReturned()
    {
        const string code = """
        func Create() -> System.ReadOnlySpan<int> {
            let values: System.Span<int> = stackalloc int[1]
            let alias: System.ReadOnlySpan<int> = values
            return alias
        }
        """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void StackAllocBackedLocal_CannotBeImplicitlyReturned()
    {
        const string code = """
        func Create() -> System.Span<int> {
            let values: System.Span<int> = stackalloc int[1]
            values
        }
        """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void SpanParameter_CanBeReturned()
    {
        const string code = """
        func Identity(values: System.Span<int>) -> System.Span<int> {
            return values
        }
        """;

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void ArrayBackedSpan_CanBeReturned()
    {
        const string code = """
        func Create(array: int[]) -> System.Span<int> {
            let values: System.Span<int> = array
            return values
        }
        """;

        CreateVerifier(code).Verify();
    }
}
