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

    [Fact]
    public void RefLikeLocal_CannotBeCapturedByLambda()
    {
        const string code = """
        unsafe func Main() -> unit {
            val values: System.Span<int> = stackalloc int[1]
            val getLength = () -> int => values.Length
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeVariableCannotBeCaptured.Id)
                .WithAnySpan()
                .WithArguments("values", "Span<int>"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void RefLikeLocal_CannotBeCapturedByLocalFunction()
    {
        const string code = """
        unsafe func Main() -> unit {
            val values: System.Span<int> = stackalloc int[1]

            func GetLength() -> int {
                values.Length
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeVariableCannotBeCaptured.Id)
                .WithAnySpan()
                .WithArguments("values", "Span<int>"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void RefLikeLocal_CannotRemainInScopeAcrossAwait()
    {
        const string code = """
        import System.Threading.Tasks.*

        unsafe async func Run() -> Task {
            val values: System.Span<int> = stackalloc int[1]
            await Task.CompletedTask
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RefLikeVariableCannotCrossAwait.Id)
                .WithAnySpan()
                .WithArguments("values", "Span<int>"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void RefLikeLocal_InCompletedNestedScope_IsAllowedBeforeAwait()
    {
        const string code = """
        import System.Threading.Tasks.*

        unsafe async func Run() -> Task {
            {
                val values: System.Span<int> = stackalloc int[1]
                val length = values.Length
            }

            await Task.CompletedTask
        }
        """;

        CreateVerifier(code).Verify();
    }
}
