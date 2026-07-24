using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RefLikeStorageDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RefLikeType_IsAllowedAsLocal()
    {
        const string code = """
        unsafe func Main() -> unit {
            val values: System.Span<int> = stackalloc int[1]
        }
        """;

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void RefLikeType_CannotBeUsedAsClassField()
    {
        const string code = """
        class Container {
            field Values: System.Span<int>
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeTypeCannotBeUsedAsField.Id)
                .WithAnySpan()
                .WithArguments("Span<int>", "Container"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void RefLikeType_CannotBackAutoProperty()
    {
        const string code = """
        class Container {
            val Values: System.Span<int>
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeTypeCannotBeUsedAsField.Id)
                .WithAnySpan()
                .WithArguments("Span<int>", "Container"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void RefLikeType_CannotBeUsedAsArrayElement()
    {
        const string code = """
        func Consume(values: System.Span<int>[]) -> unit {}
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeTypeCannotBeUsedAsArrayElement.Id)
                .WithAnySpan()
                .WithArguments("Span<int>"),
        ]);

        verifier.Verify();
    }
}
