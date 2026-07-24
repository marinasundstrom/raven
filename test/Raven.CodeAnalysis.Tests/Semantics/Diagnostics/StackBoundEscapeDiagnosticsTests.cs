using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StackBoundEscapeDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RefStructContainingReferenceToLocal_CannotBeReturned()
    {
        const string code = """
            ref struct IntReference {
                field Value: &int
            }

            func Create() -> IntReference {
                var value = 0
                var reference = IntReference()
                reference.Value = &value
                return reference
            }
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackBoundRefLikeValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void AliasContainingReferenceToLocal_CannotBeImplicitlyReturned()
    {
        const string code = """
            ref struct IntReference {
                field Value: &int
            }

            func Create() -> IntReference {
                var value = 0
                var reference = IntReference()
                reference.Value = &value
                val alias = reference
                alias
            }
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackBoundRefLikeValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void RefStructContainingReferenceParameter_CanBeReturned()
    {
        const string code = """
            ref struct IntReference {
                field Value: &int
            }

            func Create(value: &int) -> IntReference {
                var reference = IntReference()
                reference.Value = &value
                reference
            }
            """;

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void RefStructContainingStackAllocatedSpan_CannotBeReturned()
    {
        const string code = """
            ref struct SpanHolder {
                field Values: System.Span<int>
            }

            func Create() -> SpanHolder {
                var holder = SpanHolder()
                holder.Values = stackalloc int[1]
                holder
            }
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.StackAllocValueCannotEscape.Id)
                .WithAnySpan(),
        ]).Verify();
    }

    [Fact]
    public void RefStructContainingSpanParameter_CanBeReturned()
    {
        const string code = """
            ref struct SpanHolder {
                field Values: System.Span<int>
            }

            func Create(values: System.Span<int>) -> SpanHolder {
                var holder = SpanHolder()
                holder.Values = values
                holder
            }
            """;

        CreateVerifier(code).Verify();
    }
}
